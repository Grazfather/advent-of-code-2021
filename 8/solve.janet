(def entries (as-> (file/open "sampleinput") x
                   (file/read x :all)
                   (string/trim x)
                   (string/split "\n" x)))

(defn parse-entry [entry]
  (let [parts (string/split "|" entry)
        patterns (->> parts first string/trim (string/split " "))
        signals (->> parts last string/trim (string/split " "))]
    [patterns signals]))

# Part 1: Just count the number of times digits 1, 4, 7, or 8 appear.
# Since these digits have a unique # of lit segments we don't need to
# look at the patterns at all.
(var c 0)
(each entry entries
  (let [[patterns signals] (parse-entry entry)]
    (+= c (sum (map |(if (@{2 0 4 0 3 0 7 0} (length $0)) 1 0) signals)))))
(print c)

# Part 2: We have to find the pattern for each digis

# The order of the segments doesn't matter so we sort them to make sure that
# they compare as equal no matter what order they show up in.
(defn canonize [s]
  (sorted (string/bytes s)))

(defn first-contains-x-of [x digit-segs choices]
  (->> choices
       (filter (fn [pat] (= x (count (fn [c] (not (nil? (find |(= $ c) pat))))
                                     digit-segs))))
       first))

(defn array= [a1 a2]
  (and (= (length a1) (length a2))
       (= (length a1) (sum (map |(if (= $0 $1) 1 0) a1 a2)))))

(defn deduce-mapping [lmap]
  (let [digit-map @{}]
    # We know digits 1, 7, 4, and 8 strictly by the # of segments they use
    (put digit-map 1 (first (lmap 2)))
    (put digit-map 7 (first (lmap 3)))
    (put digit-map 4 (first (lmap 4)))
    (put digit-map 8 (first (lmap 7)))
    # 3 contains 5 segments and has 3 segments of 7
    (put digit-map 3 (first-contains-x-of 3 (digit-map 7) (lmap 5)))
    # 6 contains 6 segs and has 1 seg of digit 1
    (put digit-map 6 (first-contains-x-of 1 (digit-map 1) (lmap 6)))
    # 5 contains 5 segs and has 5 segs of digit 6
    (put digit-map 5 (first-contains-x-of 5 (digit-map 6) (lmap 5)))
    # 9 contains 6 segs and has 4 segs of digit 4
    (put digit-map 9 (first-contains-x-of 4 (digit-map 4) (lmap 6)))
    # 0 contains 6 segs and 4 segs of digit 5
    (put digit-map 0 (first-contains-x-of 4 (digit-map 5) (lmap 6)))
    # 2 contains 5 segs and has 3 segs of digit 5
    (put digit-map 2 (first-contains-x-of 3 (digit-map 5) (lmap 5)))
    digit-map))

(var c 0)
(each entry entries
  (let [[patterns signals] (parse-entry entry)
        len-map @{}]
    (each p patterns
      (if-let [pats (len-map (length p))]
        (put len-map (length p) [(canonize p) ;pats])
        (put len-map (length p) [(canonize p)])))

    (def digit-map (deduce-mapping len-map))
    (var cc 0)
    (each s signals
      (loop [[d pat] :pairs digit-map]
        (when (array= (canonize s) pat)
          (set cc (+ (* 10 cc) d)))))
    (+= c cc)))
(print c)
