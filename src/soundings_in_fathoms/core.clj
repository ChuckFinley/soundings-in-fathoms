(ns soundings-in-fathoms.core)

(def Tmin-depth 1)

(defn get-dive-data
	"Returns time and depth data, as from a TDR, in two vectors"
	[]
	(let [time (vec (range 17))
		  depth [0 1 2 3 2 1 0 1 2 1 0 1 2 4 2 1 0]]
		  (map (fn [t d] {:time t :depth d}) time depth)))

(defn drop-leading-dive-points
	"Drop leading points that start mid-dive. Dive data should begin with surface time."
	[dive-data]
	(drop-while #(>= (:depth %) Tmin-depth) dive-data))

(defn split-dives-at-surface
	"Splits dive data by comparing depth to the Tmin-depth threshold"
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

(defn identify-dive-indices
	"Takes dive data (time and depth vectors, e.g. from get-dive-data) and adds a third vector for dive indices. Drops leading points that are mid-dive."
	[dive-data]
	(->> dive-data
		 drop-leading-dive-points
		 split-dives-at-surface
		 assign-dive-indices
		 rejoin-dives))