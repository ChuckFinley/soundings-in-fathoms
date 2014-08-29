#Soundings in Fathoms: Data Flow

Soundings in Fathoms takes as input the data collected by a TDR. More specifically, it takes a collection of points (dive-data) where each point is a hash map with depth and time. 

The dive-data is run through a series of transformations, outputting a list of dive-partitions. Each dive-partition has a list of its points as well as the variables and statistics which describe the dive. This document describes the flow of data through these transformations.

####Input 
```
({:depth 
  :time})
```

###STEP 1: Split datapoints into dives	
####New data:
```
:dive-idx
```
####Output:
```
({:dive-idx 
  :depth 
  :time})
```

###STEP 2: Describe dives	
####New data:
```
:start-time
:end-time
:duration
:max-depth
:max-depth-time
:ledge-depth
:dive-points
```
####Output:	
```
({:dive-idx
  :start-time
  :end-time
  :duration
  :max-depth
  :max-depth-time
  :ledge-depth
  :dive-points ({:depth 
				 :time})})
```

###STEP 3: Calculate vertical velocity
####New data:
```
:vert-vel
```
####Output:	
```
({:dive-idx
  :start-time
  :end-time
  :duration
  :max-depth
  :max-depth-time
  :ledge-depth
  :dive-points ({:depth 
				 :time
				 :vert-vel})})
```

###STEP 4: Identify elements
####New data:
```
:elements ({:type
  		  :start-time
  		  :end-time
  		  :duration
  		  :min-depth
  		  :max-depth
  		  :depth-range})
```
####Output:	
```
({:dive-idx
  :start-time
  :end-time
  :duration
  :max-depth
  :max-depth-time
  :ledge-depth
  :dive-points ({:depth 
				 :time
				 :vert-vel})
  :elements	({:type
				 :start-time
				 :end-time
				 :duration
				 :min-depth
				 :max-depth
				 :depth-range})})
```

###STEP 5: Get bottom phase bounds
####New data:
```
:bottom-phase {:bottom-start-time
			   :bottom-end-time}
```
####Output:	
```
({:dive-idx
  :start-time
  :end-time
  :duration
  :max-depth
  :max-depth-time
  :ledge-depth
  :dive-points ({:depth 
				 :time
				 :vert-vel})
  :elements	({:type
				 :start-time
				 :end-time
				 :duration
				 :min-depth
				 :max-depth
				 :depth-range})
  :bottom-phase {:bottom-start-time
				 :bottom-end-time}})
```

###STEP 6: Identify phases
####New data:
```
:phase
```
####Output:
```
({:dive-idx
  :start-time
  :end-time
  :duration
  :max-depth
  :max-depth-time
  :ledge-depth
  :dive-points ({:depth 
				 :time
				 :vert-vel
				 :phase})
  :elements	({:type
				 :start-time
				 :end-time
				 :duration
				 :min-depth
				 :max-depth
				 :depth-range})
  :bottom-phase {:bottom-start-time
				 :bottom-end-time}})
```

###STEP 7: Quantify bottom phase
####New data:
```
:bottom-duration
:depth-range
:count-wiggles
```
####Output:
```
({:dive-idx
  :start-time
  :end-time
  :duration
  :max-depth
  :max-depth-time
  :ledge-depth
  :dive-points ({:depth 
				 :time
				 :vert-vel
				 :phase})
  :elements	({:type
				 :start-time
				 :end-time
				 :duration
				 :min-depth
				 :max-depth
				 :depth-range})
  :bottom-phase {:bottom-start-time
				 :bottom-end-time
				 :bottom-duration
				 :depth-range
				 :count-wiggles}})
```

####STEP 8: Describe bottom phase
####New data:
```
:broadness-idx 
:depth-range-idx 
:symmetry-idx 
:raggedness-idx
```
###Output:	
```
({:dive-idx
  :start-time
  :end-time
  :duration
  :max-depth
  :max-depth-time
  :ledge-depth
  :dive-points ({:depth 
				 :time
				 :vert-vel
				 :phase})
  :elements	({:type
				 :start-time
				 :end-time
				 :duration
				 :min-depth
				 :max-depth
				 :depth-range})
  :bottom-phase {:bottom-start-time
				 :bottom-end-time
				 :bottom-duration
				 :depth-range
				 :count-wiggles
                 :broadness-idx 
				 :depth-range-idx 
				 :symmetry-idx 
				 :raggedness-idx}})
```

###STEP 9: Categorize dive shape
####New Data:
```
:dive-shape
```
####Output:	
```
({:dive-idx
  :start-time
  :end-time
  :duration
  :max-depth
  :max-depth-time
  :ledge-depth
  :dive-shape
  :dive-points ({:depth 
				 :time
				 :vert-vel
				 :phase})
  :elements	({:type
				 :start-time
				 :end-time
				 :duration
				 :min-depth
				 :max-depth
				 :depth-range})
  :bottom-phase {:bottom-start-time
				 :bottom-end-time
				 :bottom-duration
				 :depth-range
				 :count-wiggles
                 :broadness-idx 
				 :depth-range-idx 
				 :symmetry-idx 
				 :raggedness-idx}})
```
