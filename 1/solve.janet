(def content
  (-> (file/open "input")
      (file/read :all)))

(def hs (->> content
                (string/split "\n")
                (filter (complement empty?))
                (map int/u64)))

# Part 1
(defn get-inc-count [vs]
  (sum (map (fn [a b] (if (< a b) 1 0)) vs (slice vs 1))))
(print (get-inc-count hs))

# Part 2
(def windows (map (fn [& args] (+ ;args)) hs (slice hs 1) (slice hs 2)))
(print (get-inc-count windows))
