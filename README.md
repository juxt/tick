# tick

A Clojure library for dealing with time.

## Timelines

Tick adds the concept of immutable *timelines* to java.time. A
timeline is a lazy but orderered sequence of `java.time.ZonedDateTime`
instances.

Timelines are easy to generate. One generator is `periodic-seq`, which
generates a uniform series of times separated by a fixed period.

```clojure
(require '[tick.core :as t])

;; A timeline of 15 minute intervals
(t/periodic-seq (t/clock-ticking-in-seconds) (t/minutes 15))
```

Timelines are just sequences, so can be transformed, filtered and can
have transducers applied.

Timelines can be finite or infinite.

Timelines are composeable, and `merge-timelines` takes a collection of
timelines and returns a lazy sequence of the result of their
amalgamation.

```clojure
(merge-timelines
  (bank-holiday-mondays)
  (easter-fridays))
```

## Tickers

A ticker is something that travels across a timeline.

One such ticker, useful for testing, can be created with `simulate`.

```clojure
(def simulator (t/simulate println timeline))
```

You can think of this as similar to map, but applied to timelines
rather than sequences.

Tickers can be `start`ed with a clock, and if necessary, `stop`ed.

```clojure
(start simulator (t/fixed-clock ...))
```

## Real time

A schedule is another type of ticker, executing a function across a
timeline.

```clojure
(def schedule (t/schedule println timeline))

(start schedule (t/clock-ticking-in-seconds))
```

If you want to wait for a ticker to complete its journey over a
timeline, `deref` the result of `start`.

At any time after starting a ticker you can find out its future
timeline with `timeline`, while inspecting its clock with `clock`.

## java.time

Most Clojure time libraries use clj-time, but tick uses
[**java.time**](http://www.oracle.com/technetwork/articles/java/jf14-date-time-2125367.html)
which means that it requires Java 8.

Java 8's `java.time` API is both influenced by, and superior to, Joda
Time. It is a modern immutable API that supports functional
programming. Unless you're stuck on Java 7 or ambivalent about library
choice, you should be using `java.time`.

## Discussion

The 'flow of time' complicates our systems and we can simplify them if
we separate the concept of a timeline from the 'flow' of time we
perceive.

For more information, see the author's talk at ClojuTRE-2016 on [The
Universe As A Value](https://www.youtube.com/watch?v=odPAkEO2uPQ)
where he argues that time is a perception.

## Acknowledgements

Tick is based on the same original idea as
[Chime](https://github.com/jarohen/chime). The motivation is to be
able to view timelines of remaining times while the schedule is
running. Thanks to James Henderson for this work on Chime.

## Copyright & License

The MIT License (MIT)

Copyright © 2016 JUXT LTD.

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
