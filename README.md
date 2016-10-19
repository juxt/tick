# tick

A Clojure library for dealing with time.

## java.time

Most Clojure time libraries use clj-time, but tick uses *java.time*

which means that it requires Java 8.

## Timelines

Tick adds the concept of *timelines* to java.time. A timeline is a
lazy sequence of `java.time.ZonedDateTime` instances.

Timelines are easy to generate. One generator is `periodic-seq`, which
generates a uniform series of times separated by a fixed period.

```clojure
(require '[tick.core :as t])

;; A timeline of 15 minute intervals
(t/periodic-seq (t/clock) (t/minutes 15))
```

## Schedules

Schedules are created by composition from building blocks.


## Copyright & License

The MIT License (MIT)

Copyright © 2016 JUXT LTD.

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
