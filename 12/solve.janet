(def content (-> (file/open "input")
                 (file/read :all)))


(defn add-neighbour [m node neigh]
  (if-not (nil? (m node))
    (put m node (array/push (m node) neigh))
    (put m node @[neigh]))
  m)

# Create a table of node: [neighbours]
(defn parse-grid [input]
  (def lines (->> input
                  (string/trim)
                  (string/split "\n")))
  (def m @{})
  (each line lines
    (let [[node neigh] (string/split "-" line)]
        (add-neighbour m node neigh)
        (add-neighbour m neigh node)))
  m)

(defn is-big? [node]
  (let [c (first node)]
    (and (>= c 0x41) (<= c 0x58))))

# Part 1: 'small' caves can only be visited once
(defn walk-grid-rec [grid node visited path]
  (if (= "end" node)
    (print (string/join path ","))
    (each neigh (grid node)
      (if (or (not (visited neigh)) (is-big? neigh))
        (walk-grid-rec grid neigh (merge visited @{node 0}) [;path neigh])))))

(defn walk-grid [grid]
  (walk-grid-rec grid "start" @{} @["start"]))

# Part 2: A SINGLE 'small' cave can be visited twice
(defn walk-grid-rec-2 [grid node visited path v2ed]
  (if (= "end" node)
    (print (string/join path ","))
    (each neigh (grid node)
      (let [node-visit-count (get visited node 0)
            neigh-visit-count (get visited neigh 0)
            new-v2ed (or v2ed (and (not (is-big? neigh)) (>= neigh-visit-count 1)))]
        (when (or (is-big? neigh) # We can always visit big caves
                (and (not (and v2ed (>= neigh-visit-count 1))) # We can't visit a small cave twice once we have
                     (not= "start" neigh))) # We can't revisit start
          (walk-grid-rec-2 grid neigh (merge visited @{node (inc node-visit-count)}) [;path neigh] new-v2ed))))))

(defn walk-grid-2 [grid]
  (walk-grid-rec-2 grid "start" @{} @["start"] false))

# Lazy: I just count lines in the shell
# (walk-grid (parse-grid content))
(walk-grid-2 (parse-grid content))
