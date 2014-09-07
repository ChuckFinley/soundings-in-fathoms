(ns soundings-in-fathoms.data)

(defrecord Dive [idx start end dur max-depth max-depth-time ledge shape points elements bottom-phase])

(defrecord Point [time depth vert-vel])

(defrecord Element [type start end dur min-depth max-depth range])

(defrecord BottomPhase [start end dur range n-wiggles breadth-idx range-idx symmetry-idx raggedness-idx])
