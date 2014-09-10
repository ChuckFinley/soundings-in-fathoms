(ns soundings-in-fathoms.io
	(:require [clojure-csv.core :as csv]
			  [clojure.java.io :as io]
			  [clj-time.format :as timef]
			  [clj-time.coerce :as timec]))
	
(def f1 "resources/E-5_LAT150_0867_130730_093253_00.csv")
(def f2 "resources/E-5_LAT150_0867_130821_151309_00.csv")
			  
(defn get-lotek-data 
	[f]
	(letfn [(get-time [d t]
				(timef/parse (timef/formatter "M/d/yyyy HH:mm:ss") (str d t)))]
		(->> (csv/parse-csv (io/reader f))
			 (drop 3)
			 (map #(hash-map 
				 		:time (timec/to-long (get-time (% 1) (% 2))) 
						:depth (Double/parseDouble (% 3)))))))