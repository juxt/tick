

# Naming (compared to java.time)

LocalDate => `date`

LocalDateTime => `date-time`

java.util.Date => `inst` 

js/Date => `inst`

otherwise the same as java.time

# Dates and Times

## Construction

### Now 

```clojure
(t/date), (t/zoned-date-time), (t/instant)
```

temporarily change what clock is used to get the `now` or `where` information with `with-clock`

```clojure
(t/with-clock (t/instant "2023-08-23T15:49:21.941342Z") 
   (t/date))
```

### Extraction 

for example, get the date part out of a zdt

```clojure
(t/date (t/zoned-date-time))
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
```

### from/to Strings 

```clojure
(t/parse-... "xxx", (t/formatter "pattern"))

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

### Relative 

```clojure
 (t/>> (t/instant) (t/of-hours 2))
 (t/<< ...)
```

# Amounts of Time

## Construction

```clojure
(t/of-hours 24) => Duration
(t/of-days 1) => Period
(t/of-..)
```

### From points in time

```clojure
(t/between point-a point-b)
```

## Arithmetic

```clojure
(t/+ (t/of-minutes 5) (t/of-minutes 5) (t/of-minutes 5))
(t/- ...)
```

## Extract parts

```clojure
(t/millis ...)
(t/days ...)
```

# Comparison

```clojure
t/<, t/<=, t/=, ...
t/max, t/max-by, t/min, t/min-by
```

# Predicates 

```clojure
(t/date-time? x)
```

# Units

```clojure
t/APRIL,
t/DECEMBER ...

t/FRIDAY, t/MONDAY...
```


