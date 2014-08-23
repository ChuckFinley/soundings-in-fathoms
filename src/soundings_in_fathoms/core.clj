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

(defn rejoin-dives
	"Takes a list of alternating surface and dive lists with dive indices and joins them sequentially"
	[dive-data]
	(apply concat dive-data))

;; Full step
(defn identify-dives
	"Takes dive data (time and depth vectors, e.g. from get-dive-data) and adds a third vector for dive indices. Drops leading points that are mid-dive."
	[dive-data]
	(->> dive-data
		 drop-leading-dive-points
		 split-dives-at-surface
		 assign-dive-indices
		 rejoin-dives))


;;; STEP 2: Get dive descriptions (name pending). These are the simple stats that
;;; can be calculated from dive datapoints alone - no derived variables needed (e.g.
;;; vertical velocity)

;; Substeps

;; Full step
(defn get-dive-descriptions)


;;; ALL TOGETHER NOW: Thread data through all steps and spit out dive statistics.

(defn analyze-dives
	"Takes dive data from a TDR and outputs dive statistics. See outline.txt for full list of stats."
	[dive-data]
	(->> dive-data
		 identify-dives
		 get-dive-descriptions))