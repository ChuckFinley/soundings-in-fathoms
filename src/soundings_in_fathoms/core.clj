(ns soundings-in-fathoms.core)


;;; Helper function to get data until I/O is implemented
(defn get-dive-data
	"Returns time and depth data, as from a TDR, in two vectors"
	[]
	(let [depth [0 1 2 3 2 3 4 3 4 3 2 1 0 0 0 1 2 2.2 2.4 2.6 2.8 3.0 3.2 4 4 3 2 1 0]
		  time (vec (range (count depth)))]
		  (map #(zipmap [:time :depth] [%1 %2]) time depth)))


;;; Thresholds
(def Tmin-depth 1)
(def Tledge-depth 0.75)
(def Tno-datapoints 5)
(def Tvert-vel 0.35)
(def Tbroadness 0.015)


;;; Utility functions

(defn condu [expr & clauses]
	"Like cond, but takes unary functions (hence the u in the name). Clauses must
	take the form of:
	
	unary-fn result-expr
	
	For each clause (unary-fn expr) is evaluated. If it returns logical true, the
	clause is a match and result-expr is returned. May be terminated by a single
	expression for the default case"
	(case (count clauses)
		0 (throw (Exception. "No matching conditions"))
		1 (first clauses)	; Default case
		(let [[unary-fn result-expr & next-clauses] clauses]
			(if (unary-fn expr) result-expr (apply condu expr next-clauses)))))

(defn map-dives-only
	"Takes a function and only applies it to dives (ignoring inter-dive periods)."
	[f coll]
	(let [dives 	  (filter (comp pos? :dive-idx) coll)
		  inter-dives (filter (comp neg? :dive-idx) coll)]
		(interleave inter-dives (map f dives))))

(defn bottom-element?
	"Takes a bottom phase and an element and determines if the latter is in 
	the former"
	[{:keys [bottom-start-time bottom-end-time]}
	 {:keys [start-time end-time]}]
	(and (>= start-time bottom-start-time) (<= end-time bottom-end-time)))

(defn get-bottom-elements
	"Takes a dive's bottom phase and elements. Returns only the elements in 
	the bottom phase."
	[{:keys [bottom-phase elements]}]
	(filter (partial bottom-element? bottom-phase) elements))

(defn get-bottom-wiggles
	"Get all the wiggles in the bottom phase."
	[dive-partition]
	(if-let [bottom-elements (get-bottom-elements dive-partition)]
		(filter #(= :wiggle (:type %)) bottom-elements)
		()))


;;; STEP 1: Split datapoints into dives

(letfn [(submerged? [point] (>= (:depth point) Tmin-depth))
        (surface? [point] (not (submerged? point)))
        (dive-indices [] (interleave (iterate dec -1) (iterate inc 1)))
        (assoc-dive-idx [idx partition] (hash-map :dive-idx idx :dive-points partition))]
  (defn identify-dives [dive-data]
    (->> dive-data
         (drop-while submerged?)
         (partition-by surface?)
         (map assoc-dive-idx (dive-indices)))))


;;; STEP 2: Describe dives. Associate descriptive statistics with each 
;;; dive (e.g. start-time, max-depth). See outline.txt for full list.

(letfn [(describe-dive [dive-partition]
	(let [points 		  (:dive-points dive-partition)
		  start-time 	  (:time (first points))
		  end-time 		  (:time (last points))
		  duration 		  (- end-time start-time)
		  deepest-point   (apply max-key :depth points)
		  max-depth		  (:depth deepest-point)
		  max-depth-time  (:time deepest-point)
		  ledge-depth 	  (* max-depth Tledge-depth)]
		(assoc dive-partition
			:start-time 	start-time
		 	:end-time 		end-time
		 	:duration 		duration
		 	:max-depth 		max-depth
		 	:max-depth-time	max-depth-time
		 	:ledge-depth 	ledge-depth)))]
	(defn describe-dives 
		"Associates each dive with start and end time, max depth, etc. See 
		outline.txt for full list."
		[dive-data] 
		(map describe-dive dive-data)))
					   
					   
;;; STEP 3: Calculate vertical velocity

;; Substeps
(defn vert-vel
	"Calculate the vertical velocity of a point."
	[{t1 :time d1 :depth} {t2 :time d2 :depth}]
	(/ (- d2 d1) (- t2 t1)))

(defn assoc-vert-vel
	"Given two points, associate the first point's vertical velocity."
	[[p1 p2]]
	(assoc p1 :vert-vel (vert-vel p1 p2)))

(defn assoc-final-vert-vel
	"Last point doesn't have a next point, so vertical velocity is 0."
	[last-point dive-points]
	(concat dive-points [(assoc last-point :vert-vel 0)]))

(defn dive-vert-vel
	"Given a dive partition, calculate vertical velocity at each point."
	[{:keys [dive-points] :as dive-partition}]
	(->> dive-points
		 (partition 2 1)
	 	 (map assoc-vert-vel)
	 	 (assoc-final-vert-vel (last dive-points))
		 (assoc dive-partition :dive-points)))

;; Full step
(defn calc-vert-vel
	"Calculate vertical velocity as forward slope of depth over time."
	[dive-data]
	(map dive-vert-vel dive-data))


;;; STEP 4: Identify elements

;; 4) Identify elements (start_time, end_time, type, max_depth, depth_range)
;;	a) Wiggle = three points where vertical speed passes below 0 m/s
;;	b) Step = period of at least Tno._datapoints points where 
;;	0 < vertical speed < ;; Tvert_vel

;; Substeps

(defn extract-element
	"Takes points in an element and the element type. Returns element 
	description."
	[type element]
	(let [times		 (map :time element)
		  depths	 (map :depth element)
		  start-time (first times)
		  end-time	 (last times)
		  min-depth	 (reduce min depths)
		  max-depth	 (reduce max depths)]
		{:type		  type
		 :start-time  start-time
		 :end-time	  end-time
		 :duration    (- end-time start-time)
		 :min-depth	  min-depth
		 :max-depth	  max-depth
		 :depth-range (- max-depth min-depth)}))

(defn take-wiggle-points
	"Takes three partitions - vert-vel = neg1 pos neg2 - and returns the
	points in the wiggle, namely all the points from neg1 and pos and the
	first point from neg2."
	[[neg1 pos neg2]]
	(concat neg1 pos [(first neg2)]))

(defn find-wiggles
	"Given a dive partition, find all wiggles. Wiggle = three points 
	where vertical speed passes below 0 m/s, which is equivalent to a 
	sequence of neg, pos, (first neg) vert-vel."
	[dive-partition]
	(->> dive-partition
		 (filter (comp not zero? :vert-vel))
		 (partition-by (comp pos? :vert-vel))
		 (drop 1)
		 (partition 3 2)
		 (map take-wiggle-points)
		 (map (partial extract-element :wiggle))))
	
(defn identify-wiggles
	"Takes a dive partition and adds wiggles to elements list. 
	Wiggle = three points where vertical speed passes below 0 m/s."
	[{:keys [dive-points dive-idx] :as dive-partition}]
	(if (neg? dive-idx)
		dive-partition
		(assoc dive-partition :elements (find-wiggles dive-points))))

(defn assoc-step-vel
	[dive-point]
	(let [vert-vel (:vert-vel dive-point)]
		(assoc dive-point :step-vel (and (> vert-vel 0) (< vert-vel Tvert-vel)))))

(defn valid-step?
	[step-partition]
	(and 
		((comp true? :step-vel first) step-partition) 
		(>= (count step-partition) Tno-datapoints)))

(defn find-steps
	"Given a dive partition, find all steps."
	[dive-points]
	(->> dive-points
		 (map assoc-step-vel)
		 (partition-by :step-vel)
		 (filter valid-step?)
		 (map (partial extract-element :step))))

(defn identify-steps
	"Takes a dive partition and adds steps to elements list.
	Step = period of at least Tno._datapoints points where
	0 < vertical speed < Tvert_vel"
	[{:keys [dive-points elements dive-idx] :as dive-partition}]
	(if (neg? dive-idx)
		dive-partition
		(assoc dive-partition :elements (concat elements (find-steps dive-points)))))

;; Full step
(defn identify-elements
	"Takes dive points and dive stats, finds all the elements in each dive, and
	adds them to an elements collection."
	[dive-data]
	(->> dive-data
		 (map identify-wiggles)
		 (map identify-steps)))
		 
		 
;;; STEP 5: Locate bottom phase
;;; Bottom phase begins with first element deeper than ledge_depth,  
;;;	ends with last element deeper than ledge_depth

;; Substeps
	
(defn beneath-ledge?
	"Takes a ledge and an element and determines if the latter is deeper
  	than the former"
  	[ledge-depth {max-depth :max-depth}]
  	(>= max-depth ledge-depth))

(defn find-elements-beneath-ledge
  	"Takes a dive and returns all elements beneath the ledge."
  	[{:keys [ledge-depth elements]}]
  	(seq (filter (partial beneath-ledge? ledge-depth) elements)))

(defn get-bottom-phase-bounds
	[{:keys [elements ledge-depth max-depth-time] :as dive-partition}]
	(if-let [bottom-elements (find-elements-beneath-ledge dive-partition)]
		[(-> bottom-elements first :start-time) (-> bottom-elements last :end-time)]
		[max-depth-time max-depth-time]))
			 
(defn locate-bottom-phase-in-dive
	"Gets bottom phase bounds from the first and last element deeper than 
	the ledge. Duration is the difference."
	[dive-partition]
	(let [[begin end] (get-bottom-phase-bounds dive-partition)
		  duration	  (- end begin)]
		(assoc dive-partition 
			:bottom-phase 
			{:bottom-start-time begin 
			 :bottom-end-time   end 
			 :bottom-duration   duration})))

;; Full step

(defn locate-bottom-phase
	"In each dive, locates bottom phase using elements and ledge depth."
	[dive-data]
	(map-dives-only locate-bottom-phase-in-dive dive-data))
		 
		 
;;; STEP 6: Identify phases
;;;	Descent phase is everything before bottom phase, ascent phase is  
;;;	everything after

;; Substeps

(defn identify-phase-at-point
	"Takes a point and the bottom range and returns which phase the point is in."
	[{:keys [bottom-start-time bottom-end-time]} point]
	(assoc point
		:phase
		(condu (:time point)
			#(< % bottom-start-time) :descent
			#(> % bottom-start-time) :ascent
			:bottom)))

(defn identify-phases-in-dive
	"Takes a dive partition and adds phases to dive points."
	[{:keys [bottom-phase dive-points] :as dive-partition}]
	(assoc dive-partition 
		:dive-points 
		(map (partial identify-phase-at-point bottom-phase) dive-points)))

;; Full step
(defn identify-phases
	"Associates a :phase key with each dive-point based on where it is in
	the dive relative to the bottom phase."
	[dive-data]
	(map-dives-only identify-phases-in-dive dive-data))
	
	
;;; STEP 7: Describe bottom phase

;; Substeps
(defn bottom-point?
	[{:keys [bottom-start-time bottom-end-time]} point]
	(and (>= (:time point) bottom-start-time) 
		 (<= (:time point) bottom-end-time)))

(defn get-bottom-duration
	[{{:keys [bottom-start-time bottom-end-time]} :bottom-phase}]
	(- bottom-end-time bottom-start-time))

(defn get-depth-range
	[{:keys [bottom-phase dive-points]}]
	(let [bottom-points (filter (partial bottom-point? bottom-phase) dive-points)
		  depths		(map :depth bottom-points)]
		(- (apply max depths) (apply min depths))))
		
(defn get-count-wiggles
	[dive-partition]
	(count (get-bottom-wiggles dive-partition)))
	
(defn get-broadness-idx
	"broadness_index = bottom duration / dive duration"
	[{{bottom-duration :bottom-duration} :bottom-phase
	 duration :duration}]
	(/ bottom-duration duration))

(defn get-depth-range-idx
	"depth_range_index = depth range / max depth"
	[{{depth-range :depth-range} :bottom-phase
	 max-depth :max-depth}]
	(/ depth-range max-depth))

(defn get-symmetry-idx
	"symmetry_index = (max depth time - bottom time begin) / bottom duration"
	[{max-depth-time :max-depth-time
	 {bottom-start-time :bottom-start-time
	  bottom-duration :bottom-duration} :bottom-phase}]
	  max-depth-time)

(defn get-raggedness-idx
	"raggedness_index = sum of wiggle depth ranges in bottom phase"
	[dive-partition]
	(reduce + (map :depth-range (get-bottom-wiggles dive-partition))))
	
(defn describe-bottom-phase-in-dive
	"In each dive, calculate depth_range, count_wiggles, broadness-idx, 
	depth-range-idx, symmetry-idx, and raggedness-idx."
	[dive-partition]
	(let [intermediate-step 
		(update-in dive-partition [:bottom-phase]
			assoc
			:depth-range   (get-depth-range	  dive-partition)
			:count-wiggles (get-count-wiggles dive-partition))]
		(update-in
			intermediate-step
			[:bottom-phase]
			assoc
			:broadness-idx	 (get-broadness-idx   intermediate-step)
			:depth-range-idx (get-depth-range-idx intermediate-step)
			:symmetry-idx	 (get-symmetry-idx 	  intermediate-step)
			:raggedness-idx  (get-raggedness-idx  intermediate-step))))

;; Full step
(defn describe-bottom-phase
	"Calculate the statistics which describe the bottom phase."
	[dive-data]
	(map-dives-only describe-bottom-phase-in-dive dive-data))
	
	
;;; STEP 8: Categorize dive shape

;; Substeps
(defn V-dive?
	"Checks a dive against the V shape parameters: 
	broadness_index < Tbroadness"
	[{{broadness-idx :broadness-idx} :bottom-phase}]
	(< broadness-idx Tbroadness))

(defn u-dive?
	"Checks a dive against the u shape parameters: 
	count_wiggles in bottom phase = 1"
	[{{count-wiggles :count-wiggles} :bottom-phase}]
	(= count-wiggles 1))

(defn U-dive?
	"Checks a dive against the u shape parameters: 
	count_wiggles in bottom phase >= 2"
	[{{count-wiggles :count-wiggles} :bottom-phase}]
	(>= count-wiggles 2))

(defn W-dive?
	"Checks a dive against the u shape parameters: 
	count_wiggles in bottom phase with max_depth < ledge_depth >= 1"
	[{:keys [ledge-depth] :as dive-partition}]
	(let [bottom-wiggles (get-bottom-wiggles dive-partition)
		  W-wiggles		 (filter #(< (:max-depth %) ledge-depth) bottom-wiggles)]
		(pos? (count W-wiggles))))

(defn identify-dive-shape-in-dive
	"Assoc shape category in dive shape hash."
	[dive-partition]
	(assoc dive-partition 
		:dive-shape 
		(condu dive-partition
			V-dive? :V
			u-dive? :u
			U-dive? :U
			W-dive? :W
			:undefined)))

;; Full step
(defn identify-dive-shapes
	"Identify dive shape as u, U, V, or W."
	[dive-data]
	(map-dives-only identify-dive-shape-in-dive dive-data))


;;; ALL TOGETHER NOW: Thread data through all steps and spit out dive statistics.

(defn analyze-dives
	"Takes dive data from a TDR and outputs dive statistics. See outline.txt 
	for full list of stats."
	[dive-data]
	(->> dive-data
		 identify-dives
		 describe-dives
		 calc-vert-vel
		 identify-elements
		 locate-bottom-phase
		 identify-phases
		 describe-bottom-phase
		 identify-dive-shapes))
