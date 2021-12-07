(def content (-> (file/open "input")
                 (file/read :all)))

(def initial-fishes (->> content
                         string/trim
                         (string/split ",")
                         (map scan-number)))

(defn add-fish [m fish c]
  (if (nil? (get m fish))
    (put m fish c)
    (put m fish (+ c (get m fish)))))

(defn list->map [fishes]
  (let [m @{}]
    (each f fishes
      (add-fish m f 1))
    m))

(defn elapse-day [fish-map]
  (let [new-map @{}]
    (loop [[day c] :pairs fish-map]
      (if (= 0 day)
        (do
          (add-fish new-map 6 c)
          (add-fish new-map 8 c))
        (add-fish new-map (dec day) c)))
    new-map))

(var fishes (list->map initial-fishes))
(for _ 0 256
  (set fishes (elapse-day fishes)))
(print (sum (values fishes)))
