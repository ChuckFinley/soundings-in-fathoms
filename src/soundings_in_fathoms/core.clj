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

(defn dives-only
	"Takes a function and only applies it to dives (ignoring inter-dive periods)."
	[function dive]
	(if (pos? (:dive-idx dive)) (function dive) dive))

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

;; Substeps
(defn drop-leading-dive-points
	"Drop leading points that start mid-dive. Dive data should begin with surface time."
	[dive-data]
	(drop-while #(>= (:depth %) Tmin-depth) dive-data))

(defn split-dives-at-surface
	"Splits dive data into surface and submerged periods by comparing depth to 
	the Tmin-depth threshold"
	[dive-data]
	(partition-by #(< (:depth %) Tmin-depth) dive-data))

(defn assign-dive-indices
	"Takes a list of alternating surface and dive lists, then assigns dive 
	indices. Surface period -x is followed by dive x."
	[dive-data]
	(map 
		(fn [dive-points dive-idx] (map #(assoc % :dive-idx dive-idx) dive-points))
		dive-data 
		(interleave (iterate dec -1) (iterate inc 1))))

;; Full step
(defn identify-dives
	"Takes dive data (time and depth vectors, e.g. from get-dive-data) and adds a third vector for dive indices. Drops leading points that are mid-dive."
	[dive-data]
	(->> dive-data
		 drop-leading-dive-points
		 split-dives-at-surface
		 assign-dive-indices))


;;; STEP 2: Describe dives. Associate descriptive statistics with each 
;;; dive (e.g. start-time, max-depth). See outline.txt for full list.

;; Substeps

(defn drop-idx-from-points
	"Drop dive-idx key from hashes in dive-points."
	[points]
	(map #(dissoc % :dive-idx) points))
	
(defn dive-to-hash
	"Takes a partition of dive points and returns the dive hash."
	[dive-partition]
	{:dive-idx 	  (->> dive-partition first :dive-idx)
	 :dive-points (drop-idx-from-points dive-partition)})

(defn get-max-depth-time
	"Given a list of dive points and a max depth, get the first time
	 the dive reached that depth"
	 [dive-points max-depth]
	 (->> dive-points
	 	  (filter #(= max-depth (:depth %)))
		  first
		  :time))

(defn extract-descriptions
	"Extracts descriptive stats from a dive partition."
	[dive-partition]
	(let [dive-points	  (:dive-points dive-partition)
		  times 		  (map :time dive-points)
		  depths 		  (map :depth dive-points)
		  start-time 	  (apply min times)
		  end-time 		  (apply max times)
		  duration 		  (- end-time start-time)
		  max-depth 	  (apply max depths)
		  max-depth-time  (get-max-depth-time dive-points max-depth)
		  ledge-depth 	  (* max-depth Tledge-depth)]
		(assoc dive-partition
			:start-time 	start-time
		 	:end-time 		end-time
		 	:duration 		duration
		 	:max-depth 		max-depth
		 	:max-depth-time	max-depth-time
		 	:ledge-depth 	ledge-depth)))

;; Full step
(defn partition-dives
	"Takes dive data with dive indices and partitions points into dives and
	inter-dive periods. With each dive associates start and end time, max 
	depth, etc. See outline.txt for full list."
	[dive-data]
	(->> dive-data
	 	 (map dive-to-hash)
		 (map extract-descriptions)))
					   
					   
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
		 
		 
;;; STEP 5a: Get bottom phase start and end time
;;; STEP 5b: Identify phases
;;; a) Bottom phase begins with first element deeper than ledge_depth,  
;;;	ends with last element deeper than ledge_depth
;;;	b) Descent phase is everything before bottom phase, ascent phase is  
;;;	everything after

;;; NOTE: I really don't like this because it's doing two things. Should
;;; really be refactored.

;; Substeps
	
(defn beneath-ledge?
	"Takes a ledge and an element and determines if the latter is deeper
  	than the former"
  	[ledge-depth {max-depth :max-depth}]
  	(>= max-depth ledge-depth))

(defn find-elements-beneath-ledge
  	"Takes a dive and returns all elements beneath the ledge."
  	[{:keys [ledge-depth elements]}]
  	(filter (partial beneath-ledge? ledge-depth) elements))

(defn get-bottom-phase-bounds-in-dive
	[{:keys [elements ledge-depth max-depth-time] :as dive-partition}]
	(assoc dive-partition 
		   :bottom-phase
		   (if-let [bottom-elements (seq (find-elements-beneath-ledge dive-partition))]
		   	{:bottom-start-time (->> bottom-elements first :start-time)
	 		 :bottom-end-time   (->> bottom-elements last :end-time)}
			{:bottom-start-time max-depth-time
		 	 :bottom-end-time   max-depth-time})))
			 
(defn get-bottom-phase-bounds
	[dive-data]
	(map (partial dives-only get-bottom-phase-bounds-in-dive) dive-data))

(defn label-phase-at-point
	"Takes a point and the bottom range and returns which phase the point is in."
	[{:keys [bottom-start-time bottom-end-time]} point]
	(if (< (:time point) bottom-start-time) 
		(assoc point :phase :descent)
		(if (> (:time point) bottom-end-time)
			(assoc point :phase :ascent)
			(assoc point :phase :bottom))))

(defn label-phases-in-dive
	"Takes a dive partition and adds phases to dive points."
	[{:keys [bottom-phase dive-points] :as dive-partition}]
	(assoc dive-partition 
		   :dive-points 
		   (map (partial label-phase-at-point bottom-phase) dive-points)))
	
(defn label-phases
	[dive-data]
	(map (partial dives-only label-phases-in-dive) dive-data))

;; Full steps

(defn identify-phases
	"In each dive, finds bottom phase using elements and ledge depth. Descent 
	and ascent phase are all points before and after, respectively. Only run
	on dives, not inter-dive periods."
	[dive-data]
	(->> dive-data
		 get-bottom-phase-bounds
		 label-phases))
	
	
;;; STEP 6: Describe bottom phase

;; Substeps
(defn bottom-point?
	[{:keys [bottom-start-time bottom-end-time]} point]
	(and (>= (:time point) bottom-start-time) (<= (:time point) bottom-end-time)))

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
	
(defn describe-bottom-phase-in-dive
	"Given a dive and bottom phase bounds, calculate bottom_duration, 
	depth_range, and count_wiggles."
	[dive-partition]
	(update-in dive-partition 
			   [:bottom-phase] 
			   assoc :bottom-duration (get-bottom-duration dive-partition)
			  		 :depth-range	  (get-depth-range	   dive-partition)
			   	  	 :count-wiggles	  (get-count-wiggles   dive-partition)))

;; Full step
(defn describe-bottom-phase
	"Calculate bottom_duration, depth_range, count_wiggles."
	[dive-data]
	(map (partial dives-only describe-bottom-phase-in-dive) dive-data))
	
	
;;; STEP 7: Describe dive shape
;;; broadness-idx, depth-range-idx, symmetry-idx, raggedness-idx

;; Substeps
(defn get-broadness-idx
	"broadness_index = bottom duration / dive duration"
	[{{bottom-duration :bottom-duration} :bottom-phase
	 duration :duration}]
	(/ bottom-duration duration))

(defn get-depth-range-idx
	"depth_range_index = depth_range_index = depth range / max depth"
	[{{depth-range :depth-range} :bottom-phase
	 max-depth :max-depth}]
	(/ depth-range max-depth))
	
(defn get-symmetry-idx
	"symmetry_index = (max depth time - bottom time begin) / bottom duration"
	[{max-depth-time :max-depth-time
	 {bottom-start-time :bottom-start-time
	  bottom-duration :bottom-duration} :bottom-phase}]
	  max-depth-time)
	  (comment
	"(if (pos? bottom-duration)
		(/ (- max-depth-time bottom-start-time) bottom-duration)
		0))"
		)

(defn get-raggedness-idx
	"raggedness_index = sum of wiggle depth ranges in bottom phase"
	[dive-partition]
	(reduce + (map :depth-range (get-bottom-wiggles dive-partition))))

(defn describe-dive-shape-in-dive
	"Given a dive, calculate broadness_index, depth_range_index, symmetry_index,
	and raggedness_index."
	[dive-partition]
	(assoc dive-partition 
		   :dive-shape 
		   {:broadness-idx	 (get-broadness-idx   dive-partition)
		    :depth-range-idx (get-depth-range-idx dive-partition)
		    :symmetry-idx	 (get-symmetry-idx 	  dive-partition)
		    :raggedness-idx  (get-raggedness-idx  dive-partition)}))

;; Full step
(defn describe-dive-shape
	"Calculate broadness-idx, depth-range-idx, symmetry-idx, raggedness-idx."
	[dive-data]
	(map (partial dives-only describe-dive-shape-in-dive) dive-data))


;;; STEP 8: Categorize dive shape

;; Substeps
(defn V-dive?
	"Checks a dive against the V shape parameters: 
	broadness_index < Tbroadness"
	[{{broadness-idx :broadness-idx} :dive-shape}]
	(if (< broadness-idx Tbroadness) :V))

(defn u-dive?
	"Checks a dive against the u shape parameters: 
	count_wiggles in bottom phase = 1"
	[dive-partition]
	(if (= 1 (count (get-bottom-wiggles dive-partition))) :u))

(defn U-dive?
	"Checks a dive against the u shape parameters: 
	count_wiggles in bottom phase >= 2"
	[{{count-wiggles :count-wiggles} :bottom-phase :as dive-partition}]
	(if (>= count-wiggles 2) :U))

(defn W-dive?
	"Checks a dive against the u shape parameters: 
	count_wiggles in bottom phase with max_depth < ledge_depth >= 1"
	[dive-partition]
	(let [bottom-wiggles (get-bottom-wiggles dive-partition)
		  ledge-depth	 (:ledge-depth dive-partition)
		  W-wiggles		 (filter #(< (:max-depth %) ledge-depth) bottom-wiggles)]
		(if (pos? (count W-wiggles)) :W)))

(defn undefined-dive?
	"If no other dive shape applies."
	[dive-partition]
	:undefined)

(defn get-shape-category
	"Categorize shape by:
	 a) V (broadness_index < Tbroadness)
	 b) u (count_wiggles in bottom phase = 0)
	 c) U (count_wiggles in bottom phase >= 2)
	 d) W (count_wiggles in bottom phase with max_depth < ledge_depth >= 1)
	 e) undefined"
	[dive-partition]
	(let [dive-shape-tests [V-dive? u-dive? U-dive? W-dive? undefined-dive?]]
		(first (filter identity (map #(% dive-partition) dive-shape-tests)))))

(defn categorize-dive-shape-in-dive
	"Assoc shape category in dive shape hash."
	[dive-partition]
	(assoc-in dive-partition [:dive-shape :shape-category] (get-shape-category dive-partition)))

;; Full step
(defn categorize-dive-shape
	"Categorize dive into u, U, V, or W."
	[dive-data]
	(map (partial dives-only categorize-dive-shape-in-dive) dive-data))


;;; ALL TOGETHER NOW: Thread data through all steps and spit out dive statistics.

(defn analyze-dives
	"Takes dive data from a TDR and outputs dive statistics. See outline.txt 
	for full list of stats."
	[dive-data]
	(->> dive-data
		 identify-dives
		 partition-dives
		 calc-vert-vel
		 identify-elements
		 identify-phases
		 describe-bottom-phase
		 describe-dive-shape
		 categorize-dive-shape))
