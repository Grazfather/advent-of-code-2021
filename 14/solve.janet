(def lines (as-> (file/open "input") x
                 (file/read x :all)
                 (string/trim x)
                 (string/split "\n" x)))

(defn parse-rules [lines]
  (table ;(->> lines
               (map |(string/split " -> " $))
               flatten)))

(def template (get lines 0))
(def rules (parse-rules (drop 2 lines)))

# We want to merge but add counts on collision
(defn merge+ [t1 t2]
  (def nt (table/clone t1))
  (loop [[k v] :pairs t2]
    (if-let [ov (nt k)]
      (put nt k (+ v ov))
      (put nt k v)))
  nt)

(defn get-pairs [p]
  (if (< (length p) 2)
    @{}
    (merge+ @{(take 2 p) 1} (get-pairs (drop 1 p)))))

(defn pair->pairs [p rules]
  (let [x (rules p)]
    [(string (string/from-bytes (first p)) x)
     (string x (string/from-bytes (last p)))]))

(defn add-count [ps p cnt]
  (if-let [oc (ps p)]
    (put ps p (+ cnt oc))
    (put ps p cnt)))

(defn run-step [ps rules]
  (var new-ps @{})
  (loop [[pair cnt] :pairs ps]
    (let [[l r] (pair->pairs pair rules)]
      (add-count new-ps l cnt)
      (add-count new-ps r cnt)))
  new-ps)

# Each element is part of two pairs, except for the ones on the ends in the
# template, so we compensate for them before we divide.
(defn to-counts [ps template]
  (var counts @{})
  (loop [[p c] :pairs ps]
    (let [l (string/from-bytes (first p))
          r (string/from-bytes (last p))]
      (add-count counts l c)
      (add-count counts r c)))

  (add-count counts (string/from-bytes (first template)) 1)
  (add-count counts (string/from-bytes (last template)) 1)

  (loop [[p c] :pairs counts]
    (put counts p (/ c 2)))
  counts)

# Part 1: 10 steps
(var ps (get-pairs template))
(for i 0 10
  (set ps (run-step ps rules)))

(var counts (to-counts ps template))
(print (- (max ;(values counts)) (min ;(values counts))))

# Part 2: 40 steps
(var ps (get-pairs template))
(for i 0 40
  (set ps (run-step ps rules)))

(var counts (to-counts ps template))
(print (- (max ;(values counts)) (min ;(values counts))))
