(def content
  (-> (file/open "input")
      (file/read :all)))

(def lines (->> content
               (string/split "\n")
               (filter (complement empty?))))

(def segments (map |(->> $0
                     (string/split " -> ")
                     (map |(->> $0
                                (string/split ",")
                                (map scan-number)))) lines))

(defn points-covered [seg diag?]
  (let [[start end] seg
        [x1 y1] start
        [x2 y2] end
        dx (if (> x1 x2) -1 1)
        dy (if (> y1 y2) -1 1)]
    (def points @[])
    (if (or (= x1 x2) (= y1 y2))
      # horizontal or vertical
      (each y (range y1 (+ y2 dy) dy)
        (each x (range x1 (+ x2 dx) dx)
          (array/push points [x y])))
      # diagonal. Only count if diag? is true
      (when diag?
          (var x x1)
          (var y y1)
          (while (and (not= x (+ x2 dx)) (not= y (+ y2 dy)))
            (array/push points [x y])
            (+= x dx)
            (+= y dy))))
    points))

(defn count-multi-points [segments diag?]
  (def field @{})
  (each seg segments
    (each pt (points-covered seg diag?)
      (if (nil? (get field pt))
        (put field pt 1)
        (put field pt (inc (get field pt))))))
  (sum (map |(if (< 1 $0) 1 0) field)))

# Part 1: Only consider horizontal & vertical lines
(print (count-multi-points segments false))
# Part 2: Include diagonals
(print (count-multi-points segments true))
