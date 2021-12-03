(def content
  (-> (file/open "input")
      (file/read :all)))

(def heights (->> content
                (string/split "\n")
                (filter (complement empty?))
                (map int/u64)))
(def heights2 (array 0 ;heights))
(var len (length heights))

# Part 1
(var i 1)
(var c 0)
(while (<= i len)
  (if (> (get heights i) (get heights2 i))
    (++ c))
  (++ i))
(print c)

# Part 2
(var i 1)
(var c 0)
(while (<= i (- len 3))
  (if (> (sum (slice heights i (+ i 3))) (sum (slice heights2 i (+ i 3))))
    (++ c))
  (++ i))
(print c)
