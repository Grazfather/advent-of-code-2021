(def lines (as-> (file/open "input") x
                 (file/read x :all)
                 (string/trim x)
                 (string/split "\n" x)))

# Create a set of coords that are dotted
(defn parse-dots [lines]
  (def m @{})
  (each line lines
    (let [[x y] (string/split "," line)
          [x y] (map scan-number [x y])]
        (put m [x y] true)))
  m)

(defn parse-folds [lines]
  (->> lines
       (map |(as-> $ x
                 (string/split " " x)
                 (get x 2)
                 (string/split "=" x)
                 [(first x) (scan-number (get x 1))]))))

(def m (parse-dots (take-until empty? lines)))
(def folds (parse-folds (drop-until |(string/has-prefix? "fold" $) lines)))

# Get coord of a dot after a fold
(defn get-fold-coord [[axis v] [x y]]
  (if (= axis "x")
    (do
      (if (> x v)
        [(- v (- x v)) y]
        [x y]))
    (do
      (if (> y v)
        [x (- v (- y v))]
        [x y]))))

# Return a new set of dots after performing the fold
(defn resolve-fold [dots fold]
  (def newm @{})
  (loop [dot :keys dots]
    (put newm (get-fold-coord fold dot) true))
  newm)

(defn resolve-folds [dots folds]
  (reduce (fn [acc fold] (resolve-fold acc fold)) dots folds))

# Part 1: Count dots after the first fold
(print (length (resolve-folds m (take 1 folds))))

# Part 2: Read the message after all folds
(defn print-dots [dots]
  (for j 0 6
    (for i 0 50
      (if (dots [i j])
        (prin "#")
        (prin " ")))
    (print "")))
(print-dots (resolve-folds m folds))
