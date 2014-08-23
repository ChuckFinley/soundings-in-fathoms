(ns soundings-in-fathoms.core)


;;; Helper function to get data until I/O is implemented
(defn get-dive-data
	"Returns time and depth data, as from a TDR, in two vectors"
	[]
	(let [time (vec (range 17))
		  depth [0 1 2 3 2 1 0 1 2 1 0 1 2 4 2 1 0]]
		  (map (fn [t d] {:time t :depth d}) time depth)))


;;; Thresholds
(def Tmin-depth 1)
(def Tledge-depth 0.75)


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
	(partition-by (partial :dive-idx) dive-data))

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


;;; ALL TOGETHER NOW: Thread data through all steps and spit out dive statistics.

(defn analyze-dives
	"Takes dive data from a TDR and outputs dive statistics. See outline.txt for full list of stats."
	[dive-data]
	(->> dive-data
		 identify-dives
		 describe-dives))