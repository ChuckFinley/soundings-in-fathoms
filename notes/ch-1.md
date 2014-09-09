#Chapter 1
##Of DSLs and encapsulation

###1.2.2 Extreme flexibility
There's an example of a SQL DSL using macros that lets you do something like this:

```(defn query [max]  (SELECT [a b c]    (FROM X      (LEFT-JOIN Y :ON (= X.a Y.b)))    (WHERE (AND (< a 5) (< b ~max)))))```

Is this at all applicable to SiF? Perhaps in describing input file structure (e.g. which columns are time, depth, and temperature) or defining thresholds (e.g. Tmin-depth or Tvert-vel).

###1.4.3 Most of what OOP gives you, Clojure provides
Specifically, let's look at encapsulation. Each step in SiF is divided into a 'full step' and a set of 'substeps'. The latter are functions used only by the former but are visible to the entire namespace - a classic encapsulation issue. JoC mentions three types of encapsulation:

* Namespace
* Block-level
* Local

Here's the base example:

```
(def *file-key* \a)(def *rank-key* \0)(defn- file-component [file]  (- (int file) (int *file-key*)))(defn- rank-component [rank]  (* 8 (- 8 (- (int rank) (int *rank-key*)))))(defn- index [file rank]  (+ (file-component file) (rank-component rank)))(defn lookup [board pos]  (let [[file rank] pos]    (board (index file rank))))```
This is a clever solution which takes a board in the form of a 1d vector and a position in the form of a string (e.g. "a1") and returns the piece at that position on the board. Check the text for more details. The encapsulation issues stem from those `defn-` calls: a series of functions that, though non-public, are still visible to the rest of the namespace.
**The Block-level Solution**

```(letfn [(index [file rank]          (let [f (- (int file) (int \a))                r (* 8 (- 8 (- (int rank) (int \0))))]            (+ f r)))]  (defn lookup [board pos]    (let [[file rank] pos]      (board (index file rank)))))```
This closure confused me at first because `defn` is inside `letfn`. The bindings provided at `letfn` are only visible to the block inside, but because `lookup` is bound at the namespace level, other functions can still see it. 

**The Local Solution**

```
(defn lookup2 [board pos]
  (let [[file rank] (map int pos)
        [fc rc]     (map int [\a \0])        f (- file fc)        r (* 8 (- 8 (- rank rc)))        index (+ f r)]    (board index)))
```
From an idiomatic perspective, the block-level and local solutions seem interchangeable. In this case, the local solution has a cleaner appearance and greater readability. I think choosing between the two will be implementation specific.

**Applying Encapsulation to SiF**

Let's take a look at *STEP 1: Split datapoints into dives*. There are three distinct transformations that happen here. Starting with a list of time/depth tuples, these are:

* *Drop leading dive points* - For cleanliness' sake, a dive profile  begins at the surface, not mid-dive. Any leading points deeper than the Tmin-depth threshold are discarded.
* *Split dives at the surface* - A dive begins and ends at the surface, so partition the list by comparing depth to Tmin-depth. This gives us a series of points broken into *dive* and *inter-dive* periods.
* *Assign dive indices* - Each dive gets an index so it can be individually referenced. The scheme is ordering them chronologically, with inter-dive periods getting the negative index of the following dive. Since dive profiles begin at the surface, the index pattern goes *-1, 1, -2, 2, -3, 3...*

Here's the code, before encapsulation:

```
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
```

Applying block-level encapsulation yields the following:

```
(letfn [(submerged? [point] (>= (:depth point) Tmin-depth))
        (surface? [point] (not (submerged? point)))
        (dive-indices [] (interleave (iterate dec -1) (iterate inc 1)))
        (assoc-dive-idx [idx partition] (hash-map :dive-idx idx :dive-points partition))]
  (defn identify-dives [dive-data]
    (->> dive-data
         (drop-while submerged?)
         (partition-by surface?)
         (map assoc-dive-idx (dive-indices)))))
```
Much prettier. Local encapsulation looks much the same, except `letfn` is inside `identify-dives`.

```
(defn identify-dives [dive-data]
  (letfn [(submerged? [point] (>= (:depth point) Tmin-depth))
          (surface? [point] (not (submerged? point)))
          (dive-indices [] (interleave (iterate dec -1) (iterate inc 1)))
          (assoc-dive-idx [idx partition] (hash-map :dive-idx idx :dive-points partition))]
    (->> dive-data
         (drop-while submerged?)
         (partition-by surface?)
         (map assoc-dive-idx (dive-indices)))))
```
Which one is better? Regarding local encapsulation, JoC remarks, *"Finally, we’ve now pulled all of the implementation-specific details into the body of the lookup2 function itself. This localizes the scope of the index function and all aux­iliary values to only the relevant party—lookup2."* However, isn't that also true of block-level? It's not like another function can access the functions defined in the `letfn` bindings. Correct me if I'm mistaken, but they seem equivalently encapsulated. I'll go with the former, if for no other reason than it puts the longest lines at a shallower level of indentation.