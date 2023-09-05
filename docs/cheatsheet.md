# Need to know 

* a `temporal` is an entity that relates to the timeline (LocalDate, Instant, ZonedDateTime etc)
* a `temporal-amount` is an entity representing a quantity of time - either a Duration or a Period
* a basic understanding of [the main entities of java.time](https://github.com/juxt/tick#javatime) is required to use tick
* Where tick doesn’t provide the API you need, drop to [cljc.java-time](https://github.com/henryw374/cljc.java-time)

# Naming (compared to java.time)

* LocalDate => `date`
* LocalDateTime => `date-time`
* LocalTime => `time`
* java.util.Date => `inst` 
* js/Date => `inst`

otherwise all camel-case equivalents of java.time names

# Temporals

All functions relating to `temporals` have names in the singular, whereas functions relating to `temporal-amounts` have names in the plural, e.g. `(t/hour x)` vs `(t/hours x)`

## Construction

### Now

zero-arity function for the required type 

```clojure
(t/date), (t/zoned-date-time), (t/instant), (t/...)
```

Temporarily change what clock is used to get the `now` or `where` information with `with-clock`

```clojure
(t/with-clock
  (t/zoned-date-time "2023-08-23T20:00-10:00[Pacific/Honolulu]") 
   (t/date)) 
 ; => returns (t/date "2023-08-23")
```

### Extraction / Conversion

```clojure
(t/date (t/zoned-date-time))
(t/hour (t/zoned-date-time))
(t/inst (t/zoned-date-time))
```

set hours and smaller to zero
```clojure
(t/truncate (t/instant) :hours) 
```

### Combining parts

```clojure
(-> (t/date)
    (t/at "00:00")
    (t/in "UTC"))

(-> (t/time "10:10")
    (t/on (t/date)))

; 'set' or 'adjust' a specific field
(t/with (t/date) (t/year 3030))
```

### from/to Strings 

ISO-formatted

```clojure

(t/instant "2020-02-02T00:00:00Z")
(t/... "2020...")

(str (t/instant))

```

Custom formats

```clojure
(t/parse-... "2021-...", (t/formatter "pattern"))

(t/format (t/date) (t/formatter "pattern"))
```

### from numbers 

```clojure
(t/new-date 2020 2 2) 
(t/new-... )
```
round-trip to/from epoch millis
```clojure
(-> (t/instant) (cljc.java-time.instant/to-epoch-milli) (t/instant))
```

### Relative temporals aka `shifting`

```clojure
 (t/>> (t/instant) (t/of-hours 2))
 (t/<< ...)
```

# Temporal Amounts

## Construction

```clojure
(t/of-hours 24) => Duration
(t/of-days 1) => Period
(t/of-..)
```

### From temporals

```clojure
(t/between a b)
```

## Arithmetic

```clojure
(t/+ (t/of-minutes 5) (t/of-minutes 5) (t/of-minutes 5), ...)
(t/- ...)
```

## Extract parts

function names in plural

```clojure
(t/millis ...)
(t/days ...)
```

# Comparison

```clojure
t/<, t/<=, t/=, ...
t/max, t/max-by, t/min, t/min-by
```

## contains/coincidence

```clojure 
(t/coincident? temporal-start temporal-end a-temporal))
```

# Type Predicates 

```clojure
(t/date-time? x)
(t/...? x)
```

# Units

```clojure
t/APRIL,
t/DECEMBER ...

t/FRIDAY, t/MONDAY...

(keys t/unit-map) => :nanos :days :seconds ...

```


