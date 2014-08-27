(ns soundings-in-fathoms.core)


;;; Helper function to get data until I/O is implemented
(defn get-dive-data
	"Returns time and depth data, as from a TDR, in two vectors"
	[]
	(let [depth [0 1 2 3 2 3 4 3 4 3 2 1 0 0 0 1 2 2.2 2.4 2.6 2.8 3.0 3.2 4 4 3 2 1 0]
		  time (vec (range (count depth)))]
		  (map (fn [t d] {:time t :depth d}) time depth)))


;;; Thresholds
(def Tmin-depth 1)
(def Tledge-depth 0.75)
(def Tno-datapoints 5)
(def Tvert-vel 0.35)


;;; Utility functions

(defn dives-only
	"Takes a function and only applies it to dives (ignoring inter-dive periods)."
	[function dive]
	(if (pos? (:dive-idx dive)) (function dive) dive))

(defn get-bottom-elements
	"Takes a dive and its elements. Returns only the elements in the bottom phase."
	[{:keys [ledge-depth elements]}]
	(seq (filter #(>= (:min-depth %) ledge-depth) elements)))


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
		 
		 
;;; STEP 5: Identify phases
;;; a) Bottom phase begins with first element deeper than ledge_depth,  
;;;	ends with last element deeper than ledge_depth
;;;	b) Descent phase is everything before bottom phase, ascent phase is  
;;;	everything after

;; Substeps

(defn label-phase-at-point
	"Takes a point and the bottom range and returns which phase the point is in."
	[{:keys [bottom-begin bottom-end]} point]
	(let [time (:time point)]
		(if (< time bottom-begin) (assoc point :phase :descent)
		(if (> time bottom-end)	  (assoc point :phase :ascent)
								  (assoc point :phase :bottom)))))
	
(defn get-bottom-phase-times
	"Takes elements, ledge-depth, and max-depth-time and returns the start and
	end of the bottom phase. If there is no bottom phase, then max-depth-time
	marks the barrier."
	[{:keys [elements ledge-depth max-depth-time] :as dive}]
	(if-let [bottom-elements (get-bottom-elements dive)]
		{:bottom-begin (->> bottom-elements first :start-time)
		 :bottom-end   (->> bottom-elements last :end-time)}
		{:bottom-begin max-depth-time
		 :bottom-end   max-depth-time}))

(defn label-phases-in-dive
	"Takes a dive partition and adds phases to dive points."
	[{:keys [dive-points elements ledge-depth max-depth-time] :as dive-partition}]
	(let [bottom-bounds (get-bottom-phase-times dive-partition)]
		(let [labeled-points 
			  (map (partial label-phase-at-point bottom-bounds) dive-points)]
			(assoc dive-partition :dive-points labeled-points))))

;; Full step

(defn identify-phases
	"In each dive, finds bottom phase using elements and ledge depth. Descent 
	and ascent phase are all points before and after, respectively. Only run
	on dives, not inter-dive periods."
	[dive-data]
	(map (partial dives-only label-phases-in-dive) dive-data))
	
	
;;; STEP 6: Describe bottom phase

;; Substeps
(defn describe-bottom-phase-in-dive
	"Given a dive, calculate bottom_start_time, bottom_end_time, bottom_duration,
	depth_range, and count_wiggles."
	[{:keys [dive-points elements ledge-depth] :as dive-partition}]
	(let [bottom-points 	(filter #(= :bottom (:phase %)) dive-points)
		  bottom-start-time (apply min (map :time bottom-points))
		  bottom-end-time	(apply max (map :time bottom-points))
		  bottom-duration	(- bottom-end-time bottom-start-time)
		  depths			(map :depth bottom-points)
		  depth-range		(- (apply max depths) (apply min depths))]
		(assoc dive-partition :bottom-phase 
			{:bottom-start-time bottom-start-time
			 :bottom-end-time	bottom-end-time
			 :bottom-duration	bottom-duration
			 :depth-range		depth-range
			 :count-wiggles
			 	(if-let [bottom-elements (get-bottom-elements dive-partition)]
					(count (filter #(= :wiggle %) (map :type bottom-elements)))
					0)})))

;; Full step
(defn describe-bottom-phase
	"Calculate bottom_start_time, bottom_end_time, bottom_duration, depth_range."
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
	(if (pos? bottom-duration)
		(/ (- max-depth-time bottom-start-time) bottom-duration)
		0))

(defn get-raggedness-idx
	"raggedness_index = sum of wiggle depth ranges in bottom phase"
	[dive-partition]
	(let [bottom-elements (get-bottom-elements dive-partition)
		  bottom-wiggles  (filter #(= :wiggle (:type %)) bottom-elements)
		  depth-ranges	  (map :depth-range bottom-wiggles)]
		(reduce + depth-ranges)))

(defn describe-dive-shape-in-dive
	"Given a dive, calculate broadness_index, depth_range_index, symmetry_index,
	and raggedness_index."
	[dive-partition]
	  (assoc dive-partition :dive-shape 
		  {:broadness-idx	(get-broadness-idx 	 dive-partition)
		   :depth-range-idx (get-depth-range-idx dive-partition)
		   :symmetry-idx	(get-symmetry-idx 	 dive-partition)
		   :raggedness-idx 	(get-raggedness-idx  dive-partition)}))

;; Full step
(defn describe-dive-shape
	"Calculate broadness-idx, depth-range-idx, symmetry-idx, raggedness-idx."
	[dive-data]
	(map (partial dives-only describe-dive-shape-in-dive) dive-data))


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
		 describe-dive-shape))
