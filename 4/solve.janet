(def content
  (-> (file/open "input")
      (file/read :all)))

(var lines (->> content
               (string/split "\n")
               (filter (complement empty?))))

(def nums (->> (first lines)
               (string/split ",")
               (map int/u64)))
(pp nums)
(print "\n")
(set lines (drop 1 lines))

(defn read-game [lines]
  (let [rows (take 5 lines)
        rem (drop 5 lines)
        rows (map |(->> $0
                        (string/split " ")
                        (filter (complement empty?))
                        (map int/u64)) rows)]
    (var t @{})
    (var i 0)
    (each row rows
      (var j 0)
      (each v row
        # Store the row and col where this number is, + whether it's been seen
        (put t v [i j false])
        (++ j))
      (put t (+ 1000 i) 0) # Store a count of scored numbers in each row
      (put t (+ 2000 i) 0) # Store a count of scored numbers in each column
      (++ i))
    [t rem]))

(defn count-unmarked [b]
  (->> b
       (filter |((= false (get $1 2))))
       sum))
  #(var total 0)
  #(loop [[value rcs] :pairs b]
    ## Some keys are the row/col count, but since their keys aren't lists we can
    ## ignore
    #(when (false? (get rcs 2))
      #(+= total value)))
  #total)

(defn score-board [b v]
  (when-let [[row col] (b v)]
    (put b v [row col true]) # Mark as seen
    # Update counts for each row & col
    (put b (+ 1000 row) (inc (b (+ 1000 row))))
    (put b (+ 2000 col) (inc (b (+ 2000 col))))
    (when (or (= 5 (b (+ 1000 row))) (= 5 (b (+ 2000 col))))
      (count-unmarked b))))

(var boards @[])
(while (not (empty? lines))
  (let [[b rem] (read-game lines)]
    (array/push boards b)
    (print "Board " (length boards))
    (pp b)
    (print "")
    (set lines rem)))

# Part 1: Find the winner
# Part 2: Find the last one to win
(var won @{})
(each turn nums
  (each b boards
    (if (not (won b))
      (when-let [score (score-board b turn)]
        (print "We have a winner after playing " turn " with score " score)
        (put won b true)
        (print (* turn score))))))
