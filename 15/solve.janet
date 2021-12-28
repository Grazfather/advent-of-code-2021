(def lines (as-> (file/open "input") x
                 (file/read x :all)
                 (string/trim x)
                 (string/split "\n" x)))

(def wx (length lines))
(def endx (dec wx))

(defn parse-graph [lines]
  (def g @{})
  (var i 0)
  (each line lines
    (var j 0)
    (each v line
      (put g [i j] (- v 0x30))
      (++ j))
    (++ i))
  g)

(defn get-neighbours [[x y] g]
  (reduce (fn [acc [dx dy]] (if (not (nil? (g [(+ x dx) (+ y dy)])))
                              [[(+ x dx) (+ y dy)] ;acc]
                              acc))
          [] [[0 -1] [-1 0] [1 0] [0 1]]))

# Dijksta's algorithm is fast enough for part 1
(defn find-shortest [start end g]
  # Build visited map
  (def v @{})
  (loop [p :keys g]
    (put v p false))

  # Build tentative distance table
  (def td @{})
  (loop [p :keys g]
    (if (= p start)
      (put td p 0)
      (put td p math/inf)))

  (def uv @{})
  (loop [p :keys g]
    (put uv p true))

  (var done false)
  (var c start)
  (while (not done)
    (each neigh (get-neighbours c g)
      (when (< (+ (td c) (g neigh)) (td neigh))
        (put td neigh (+ (td c) (g neigh)))))
    # Mark as visited
    (put v c true)
    # Remove from unvisited
    (put uv c nil)
    # See if we are done
    (when (= c end)
      (set done true))
    # Find a new current node
    (var mp nil)
    (var mn math/inf)
    (loop [p :keys uv]
      (if (= false (v p))
        (when (< (td p) mn)
          (set mn (td p))
          (set mp p))))
    (set c mp))

  (td end))

# Part 1: Get the total risk of the least-risky path
(def g (parse-graph lines))
# We know it's a square
(def end [endx endx])
(print (find-shortest [0 0] end g))

# Part 2: Get the total risk of the least-risky path, where the math repeats 5x
# in each dimension, with increasing risk, and where risk levels of 9 loop back
# to 1.
(defn risk-up [g]
  (def ng @{})
  (loop [[p r] :pairs g]
    (put ng p (inc (mod r 9))))
  ng)

(defn loop-diff [v i]
  (var v v)
  (for _ 0 i
    (set v (inc (mod v 9))))
  v)

(defn adjust-coords [g dx dy]
  (def ng @{})
  (loop [[[x y] r] :pairs g]
    (put ng [(+ x dx) (+ y dy)] r))
  ng)

(defn repeat-grid [g n w]
  (let [x (->> (range n)
               (map (fn [i] (map (fn [j] [i j]) (range n))))
               flatten
               (partition 2)
               (map (fn [[i j]] (map (fn [[[x y] d]] [[(+ x (* w i)) (+ y (* w j))] (loop-diff d (+ i j))]) (pairs g))))
               )]
    (var ng @{})
    (each mg x
      (each [p v] mg
      (put ng p v)))
    ng))

# Use a simple heuristic: Taxi cab distance. We just can't over estimate.
(defn heur [n end]
  (+ (sum n) (sum end)))

(defn find-shortest-a* [start end g heur]
  (def open-set @{start true})
  (def g-score @{start 0})
  (def f-score @{start (heur start end)})
  (var done false)
  (var c start)
  (while (and (not (empty? open-set)) (not done))
    # Find a new current node
    (var mp nil)
    (var mn math/inf)
    (loop [p :keys open-set]
      (when (< (f-score p) mn)
        (set mn (f-score p))
        (set mp p)))
    (set c mp)

    # We're done once we reach the dest
    (when (= c end)
      (set done true))

    # Remove current
    (put open-set c nil)

    (each neigh (get-neighbours c g)
      (let [tgs (+ (g-score c) (g neigh))]
        (when (< tgs (g-score neigh))
          (put g-score neigh tgs)
          (put f-score neigh (+ tgs (heur neigh end)))
          (when (nil? (open-set neigh))
            (put open-set neigh true))))))
  (g-score end))

(print (find-shortest-a* [0 0] [(dec (* 5 wx)) (dec (* 5 wx))] (repeat-grid g 5 wx) heur))
