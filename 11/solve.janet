(def content (-> (file/open "input")
                 (file/read :all)))


# Create a table of (x, y): energy level
(defn parse-grid [input]
  (def lines (->> input
                  (string/trim)
                  (string/split "\n")))
  (def m @{})
  (var i 0)
  (while (< i (length lines))
    (var j 0)
    (while (< j (length (first lines)))
      (put m [i j] (- (get (get lines i) j) 0x30))
      (++ j))
    (++ i))
  m)

# Get all valid neighbour coordinates of provided position
(defn get-neighbours [[x y] m]
  (def neighbours @[])
  (loop [[dx dy] :in [[-1 -1] [0 -1] [1 -1]
                      [-1  0]        [1  0]
                      [-1  1] [0  1] [1  1]]
         :let [nx (+ x dx)
               ny (+ y dy)]
         :when (not (nil? (m [nx ny])))]
    (array/push neighbours [nx ny]))
  neighbours)

(defn inc-power [pos m]
  (let [e (m pos)
        ne (inc e)]
    (put m pos ne)
    (if (= ne 10)
      (->> (get-neighbours pos m)
           (map |(inc-power $ m))
           sum
           inc)
      0)))

# Reset all flashed octopuses to 0
(defn reset-power [m]
  (loop [[pos e] :pairs m]
    (if (> e 9)
      (put m pos 0))))

# Return a function that closes over a grid and runs one step
(defn get-stepfunc [m]
  (fn [_]
    (let [c (->> (keys m)
                 (map |(inc-power $ m))
                 sum)]
      (reset-power m)
      c)))

# Part 1: Count the number of flashes after 100 steps
(def m (parse-grid content))
(->> (range 100)
     (map (get-stepfunc m))
     sum
     print)

# Part 2: Find the first day where all octopuses flash
(def m (parse-grid content))
(->> (range 300) # No infinite range/transducers?
     (map (get-stepfunc m))
     (take-until |(= $ 100))
     length
     inc
     print)
