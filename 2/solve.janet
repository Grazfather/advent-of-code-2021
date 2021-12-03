(def content
  (-> (file/open "input")
      (file/read :all)))

(def cmds (->> content
               (string/split "\n")
               (filter (complement empty?))))

# Part 1
(defn adjust [[dpos hpos] cmd]
  (let [[cmd val] (string/split " " cmd)
        val (int/u64 val)]
    (cond
      (= cmd "forward") [dpos (+ hpos val)]
      (= cmd "down") [(+ dpos val) hpos]
      (= cmd "up") [(- dpos val) hpos])))

(print (let [[d p] (reduce adjust [0 0] cmds)]
      (* d p)))

# Part 2
(defn adjust2 [[dpos hpos aim] cmd]
  (let [[cmd val] (string/split " " cmd)
        val (int/u64 val)]
    (cond
      (= cmd "forward") [(+ dpos (* aim val)) (+ hpos val) aim]
      (= cmd "down") [dpos hpos (+ aim val)]
      (= cmd "up") [dpos hpos (- aim val)])))

(print (let [[d p a] (reduce adjust2 [0 0 0] cmds)]
         (* d p)))
