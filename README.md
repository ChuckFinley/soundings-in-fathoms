# Soundings in Fathoms
Temperature-depth recorders (TDRs) are devices that can be attached to marine animals to collect diving behavior observations. It is necessary to have an objective and cost-effective method for analyzing their output. Quoting Halsey, Bost, and Handrich:
>To make full use of the large data sets now available on diving behaviour, it is rarely feasible to categorise dive data manually, i.e. dive by dive. Also, such comparisons based on visual assessment of dive profiles are subjective and affected by reproducibility error.
[http://www.cebc.cnrs.fr/publipdf/2007/HPB30.pdf]

This project is an implementation of the algorithm authored in the  paper mentioned above. It is intended to be a simple-to-use tool for quickly analyzing loads of TDR output. Granted, at present it's a command-line based Clojure program, but eventually it'll be wrapped in a user-friendly, accessible interface.

## Usage

```clojure 
(analyze-dives (get-dive-data))
```
`analyze-dives` is the analysis command and `get-dive-data` supplies sample data. To supply other data, provide a collection of hashes with `:depth` and `:time` keys.

See https://github.com/ChuckFinley/soundings-in-fathoms/blob/master/doc/DataFlow.md for details.

## The MIT License (MIT)

Copyright (c) 2014 Max Czapanskiy

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.




