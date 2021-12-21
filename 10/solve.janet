(def content (-> (file/open "input")
                 (file/read :all)))

(def lines (->> content
                (string/trim)
                (string/split "\n")))

(def l1 (first lines))

(def openers @{"[" 0 "(" 0 "{" 0 "<" 0})
(def scores @{"[" 57 "(" 3 "{" 1197 "<" 25137
              "]" 2 ")" 1 "}" 3 ">" 4})
(def counterpart @{"]" "[" ")" "(" "}" "{" ">" "<"
                   "[" "]" "(" ")" "{" "}" "<" ">"})

# Part 1: Score based on the first error of each line
(def all-errors @[])
(each l lines
  (def stack @[])
  (def errors @[])
  (each c l
    (let [c (string/from-bytes c)]
      (if-not (nil? (openers c))
        (array/push stack c)
        (if (= (array/peek stack) (counterpart c))
          (array/pop stack)
          (array/push errors (scores (counterpart c)))))))
  (if (not (empty? errors))
    (array/push all-errors (first errors))))

(print (sum all-errors))

# Part 2: For incomplete lines, scored based on the sequence to close them
(def line-scores @[])
(each l lines
  (def stack @[])
  (var bad false)
  (each c l
    (let [c (string/from-bytes c)]
      (if-not (nil? (openers c))
        (array/push stack c)
        (if (= (array/peek stack) (counterpart c))
          (array/pop stack)
          (set bad true)))))
  (if (not bad)
    (array/push line-scores
                (reduce (fn [acc v]
                          (+ (* 5 acc) (scores v)))
                        0 (map |(counterpart $) (reverse stack))))))

(print (as-> line-scores x
             (sorted x)
             (drop (math/floor (/ (length x) 2)) x)
             (first x)))
