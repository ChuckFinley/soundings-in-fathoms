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


;;; STEP 1: Split datapoints into dives

;; Substeps
(defn drop-leading-dive-points
	"Drop leading points that start mid-dive. Dive data should begin with surface time."
	[dive-data]
	(drop-while #(>= (:depth %) Tmin-depth) dive-data))

(defn split-dives-at-surface
	"Splits dive data into surface and submerged periods by comparing depth to the Tmin-depth threshold"
	[dive-data]
	(partition-by #(< (:depth %) Tmin-depth) dive-data))

(defn assign-dive-indices
	"Takes a list of alternating surface and dive lists, then assigns dive indices. Surface period -x is followed by dive x."
	[dive-data]
	(map (fn [dive-points dive-idx] (map #(assoc % :dive-idx dive-idx) dive-points)) dive-data (interleave (iterate dec -1) (iterate inc 1))))

;; Full step
(defn identify-dives
	"Takes dive data (time and depth vectors, e.g. from get-dive-data) and adds a third vector for dive indices. Drops leading points that are mid-dive."
	[dive-data]
	(->> dive-data
		 drop-leading-dive-points
		 split-dives-at-surface
		 assign-dive-indices
		 flatten))


;;; STEP 2: Get dive descriptions (name pending). These are the simple stats that
;;; can be calculated from dive datapoints alone - no derived variables needed (e.g.
;;; vertical velocity). See outline.txt for full list.

;; Substeps
(defn discard-surface-periods
	"Takes the list of dive points and drops surface periods"
	[dive-data]
	(filter #(pos? (:dive-idx %)) dive-data))
	
(defn partition-dives
	"Partitions the dives into nested lists."
	[dive-data]
	(partition-by :dive-idx dive-data))

(defn get-max-depth-time
	"Given a list of dive points and a max depth, get the first time
	 the dive reached that depth"
	 [dive-points max-depth]
	 (->> dive-points
	 	  (filter #(= max-depth (:depth %)))
		  (first)
		  (:time)))

(defn extract-descriptions
	"Extracts descriptive stats from a dive partition."
	[dive-partition]
	(let [times (map :time dive-partition)
		  depths (map :depth dive-partition)
		  dive-idx (:dive-idx (first dive-partition))
		  start-time (apply min times)
		  end-time (apply max times)
		  duration (- end-time start-time)
		  max-depth (apply max depths)
		  max-depth-time (get-max-depth-time dive-partition max-depth)
		  ledge-depth (* max-depth Tledge-depth)]
		 {:dive-idx dive-idx
		  :start-time start-time
		  :end-time end-time
		  :duration duration
		  :max-depth max-depth
		  :max-depth-time max-depth-time
		  :ledge-depth ledge-depth}))

(defn get-dive-descriptions
	"Given dive-partitions, maps extract-descriptions to get
	descriptive stats."
	[dive-partitions]
	(map extract-descriptions dive-partitions))

;; Full step
(defn describe-dives
	"Takes dive data with dive indices and begins building the dive stats vector
	with start and end time, max depth, etc. See outline.txt for full list."
	[dive-data]
	{:dive-points dive-data
	 :dive-stats  (->> dive-data
	 				   discard-surface-periods
					   partition-dives
					   get-dive-descriptions)})
					   
					   
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

(defn map-vert-vel
	"Given the list of dive points, calculate vertical velocity at each point."
	[dive-points]
	(->> dive-points
		 (partition 2 1)
	 	 (map assoc-vert-vel)
	 	 (assoc-final-vert-vel (last dive-points))))

;; Full step
(defn calc-vert-vel
	"Calculate vertical velocity as forward slope of depth over time."
	[{:keys [dive-points dive-stats]}]
	{:dive-points (map-vert-vel dive-points)
	 :dive-stats dive-stats})


;;; STEP 4: Identify elements

;; 4) Identify elements (start_time, end_time, type, max_depth, depth_range)
;;	a) Wiggle = three points where vertical speed passes below 0 m/s
;;	b) Step = period of at least Tno._datapoints points where 0 < vertical speed < ;; Tvert_vel

;; Substeps

(defn extract-element
	"Takes partitions of points in an element and the element type. Returns element description."
	[type element]
	(let [e 	 	 (flatten element)
		  times		 (map :time e)
		  depths	 (map :depth e)
		  start-time (first times)
		  end-time	 (last times)
		  min-depth	 (apply min depths)
		  max-depth	 (apply max depths)]
		{:type		  type
		 :dive-idx	  (:dive-idx (first e))
		 :start-time  start-time
		 :end-time	  end-time
		 :duration    (- end-time start-time)
		 :min-depth	  min-depth
		 :max-depth	  max-depth
		 :depth-range (- max-depth min-depth)}))

(defn find-wiggles-in-dive
	"Given a dive partition, find all wiggles. Wiggle = three points where vertical speed passes below 0 m/s, which is equivalent to a sequence of neg, pos, neg vert-vel."
	[dive-partition]
	(->> dive-partition
		 (filter (comp not zero? :vert-vel))
		 (partition-by (comp pos? :vert-vel))
		 (drop 1)
		 (partition 3 2)
		 (map (partial extract-element :wiggle))))
	
(defn find-wiggles
	"Find wiggles in each dive. Wiggle = three points where vertical speed passes below 0 m/s."
	[dive-partitions]
	{:dive-partitions	dive-partitions
	 :elements			(->> dive-partitions
	 						 (map find-wiggles-in-dive)
							 flatten)})

(defn assoc-step-vel
	[dive-point]
	(let [vert-vel (:vert-vel dive-point)]
		(assoc dive-point :step-vel (and (> vert-vel 0) (< vert-vel Tvert-vel)))))

(defn valid-step?
	[step-partition]
	(and 
		((comp true? :step-vel first) step-partition) 
		(>= (count step-partition) Tno-datapoints)))

(defn find-steps-in-dive
	"Given a dive partition, find all steps."
	[dive-partition]
	(->> dive-partition
		 (map assoc-step-vel)
		 (partition-by :step-vel)
		 (filter valid-step?)
		 (map (partial extract-element :step))))

(defn find-steps
	"Find steps in each dive. Step = period of at least Tno._datapoints points where 0 < vertical speed < Tvert_vel"
	[{:keys [dive-partitions elements]}]
	(concat elements (->> dive-partitions
						  (map find-steps-in-dive)
						  flatten)))
	
(defn get-elements
	"Takes the dive points and returns a collection of elements."
	[dive-points]
	(->> dive-points
		 (filter (comp pos? :dive-idx))
		 (partition-by :dive-idx)
		 (find-wiggles)
		 (find-steps)))

;; Full step
(defn identify-elements
	"Takes dive points and dive stats, finds all the elements in each dive, and
	adds them to an elements collection."
	[{:keys [dive-points dive-stats]}]
	{:dive-points	dive-points
	 :elements		(get-elements dive-points)
	 :dive-stats	dive-stats})



;;; ALL TOGETHER NOW: Thread data through all steps and spit out dive statistics.

(defn analyze-dives
	"Takes dive data from a TDR and outputs dive statistics. See outline.txt for full list of stats."
	[dive-data]
	(->> dive-data
		 identify-dives
		 describe-dives
		 calc-vert-vel
		 identify-elements))
