(def content (-> (file/open "input")
                 (file/read :all)))

(def pos (->> content
              string/trim
              (string/split ",")
              (map scan-number)))

(defn get-best-pos [pos cost-fn]
  (var tots @[])
  (for i (min ;pos) (max ;pos)
    (var tot 0)
    (each p pos
      (let [dist (math/abs (- i p))]
        (+= tot (cost-fn dist))))
    (array/push tots [tot i]))
  (first (sort-by (fn [v] (first v)) tots)))

# Part 1: 1 fuel consumed per value.
(pp (get-best-pos pos identity))

# Part 2: Fuel goes up by 1 for each step (triangular number)
(defn cost-part2 [dist] (/ (* dist (inc dist)) 2))
(pp (get-best-pos pos cost-part2))
