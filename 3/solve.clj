(def content (slurp "input"))

(def lines (as-> content v
               (str/split v #"\n")
               (filter (complement empty?) v)))
(defn binstr->bits [s] (mapv (fn [v] (if (= \1 v) 1 0)) s))
(def lines-bits (map binstr->bits lines))
(def num-bits (count (nth lines-bits 0)))

(defn bits->val [bits]
  (reduce (fn [acc v] (+ (* 2 acc) v)) 0 bits))

(defn ones-win [net] (if (neg? net) 0 1))
(defn zeros-win [net] (if (neg? net) 1 0))

; Part 1
(defn countbits
  "For each entry in each arg it adds 1 for 1 bits and subs 1 for negative
  bits. To be used in a recuder to get a net balance of bits at each location."
  [counts vs]
  (mapv (fn [a b] (+ a (if (= 1 b) 1 -1))) counts vs))

(defn flipbits [bits]
  (map (partial bit-xor 1) bits))

(println (let [gamma (map ones-win
                          (reduce countbits (repeat num-bits 0) lines-bits))
               epsilon (bits->val (flipbits gamma))
               gamma (bits->val gamma)]
           (* gamma epsilon)))

; Part 2
(defn bit-balance
  "For each bit in bits it adds 1 for 1 bits and subs 1 for negative bits."
  [bits]
  (reduce (fn [acc v] (+ acc (if (= 1 v) 1 -1))) 0 bits))

(defn filter-bits
  "filters a vector of bit vectors recursively bit by bit by using the vfn on
  the 'bit balance' to determine which value to match against."
  [vfn vbits]
  (loop [i 0
         vs vbits]
    (if (= (count vs) 1)
      (nth vs 0)
      (let [bits-at-i (map (fn [v] (nth v i)) vs)
            bb (bit-balance bits-at-i)
            cmpv (vfn bb)]
        (recur (inc i) (filter (fn [v] (= (nth v i) cmpv)) vs))))))

(println
  (let [o2-rating (bits->val (filter-bits ones-win lines-bits))
        co2-rating (bits->val (filter-bits zeros-win lines-bits))]
    (* o2-rating co2-rating)))
