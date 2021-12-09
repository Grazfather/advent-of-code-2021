(def content (-> (file/open "input")
                 (file/read :all)))

(def lines (->> content
                (string/trim)
                (string/split "\n")))

# Create a table of (x, y): height
(def m @{})
(var i 0)
(while (< i (length lines))
  (var j 0)
  (while (< j (length (first lines)))
    (put m [i j] (- (get (get lines i) j) 0x30))
    (++ j))
  (++ i))

# Get all valid neighbour coordinates of provided position
(defn get-neighbours [[x y] m]
  (def neighbours @[])
  (loop [[dx dy] :in [[0 -1] [-1 0] [1 0] [0 1]]
         :let [nx (+ x dx)
               ny (+ y dy)]
         :when (not (nil? (m [nx ny])))]
    (array/push neighbours [nx ny]))
  neighbours)

# If I want to avoid explicitly modifying an array:
(defn get-neighbours [[x y] m]
  (reduce (fn [acc [dx dy]] (if (not (nil? (m [(+ x dx) (+ y dy)])))
                              [[(+ x dx) (+ y dy)] ;acc]
                              acc))
          [] [[0 -1] [-1 0] [1 0] [0 1]]))

(defn is-lowpoint? [pos m]
  (all |(< (m pos) (m $0)) (get-neighbours pos m)))

# Part 1: Get the sum of risk levels (1 + height) of each low point
(print (reduce (fn [acc [pos v]]
                 (if (is-lowpoint? pos m) (+ acc (inc v)) acc))
               0 (pairs m)))

# Part 2: Get the product of the size of the three largest basins
# Since we know that each location is only part of one basin we can find each
# basin by its low point.
(defn add-to-basin-rec [points pos m]
  (when (and (nil? (points pos)) (not= (m pos) 9))
    (put points pos 0)
    (each neigh (get-neighbours pos m)
      (add-to-basin-rec points neigh m))))

(def basin-sizes @[])
(loop [[pos v] :pairs m :when (is-lowpoint? pos m)]
  (def basin-points @{})
  (add-to-basin-rec basin-points pos m)
  (array/push basin-sizes (length basin-points)))

(print (product (take 3 (sorted basin-sizes >))))
