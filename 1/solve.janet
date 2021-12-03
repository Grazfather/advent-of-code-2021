(def content
  (-> (file/open "input")
      (file/read :all)))

(def hs (->> content
                (string/split "\n")
                (filter (complement empty?))
                (map int/u64)))
(def len (length hs))

# Part 1
(print
  (sum (map (fn [a b] (if (< a b) 1 0)) hs (slice hs 1))))

# Part 2
(def windows (map (fn [a b c] (+ a b c)) hs (slice hs 1) (slice hs 2)))
(print (sum (map (fn [a b] (if (< a b) 1 0)) windows (slice windows 1))))
