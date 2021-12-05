(def content
  (-> (file/open "input")
      (file/read :all)))

(def lines (->> content
               (string/split "\n")
               (filter (complement empty?))))

(defn binstr->bits [s] (map (fn [v] (if (= 0x31 v) 1 0)) s))
(def lines-bits (map binstr->bits lines))
(def num-bits (length (get lines-bits 0)))

(defn bits->val [bits]
  (reduce (fn [acc v] (+ (* 2 acc) v)) 0 bits))

(defn ones-win [net] (if (neg? net) 0 1))
(defn zeros-win [net] (if (neg? net) 1 0))

# Part 1
(defn countbits
  "For each entry in each arg it adds 1 for 1 bits and subs 1 for negative
  bits. To be used in a recuder to get a net balance of bits at each location."
  [counts vs]
  (map (fn [a b] (+ a (if (= 1 b) 1 -1))) counts vs))

(defn flipbits [bits]
  (map (partial bxor 1) bits))

(print (let [gamma (map ones-win
                        (reduce countbits (array/new-filled num-bits 0) lines-bits))
             epsilon (bits->val (flipbits gamma))
             gamma (bits->val gamma)]
         (* gamma epsilon)))

# Part 2
(defn bit-balance
  "For each bit in bits it adds 1 for 1 bits and subs 1 for negative bits."
  [bits]
  (reduce (fn [acc v] (+ acc (if (= 1 v) 1 -1))) 0 bits))

(defn filter-bits
  "filters a vector of bit vectors recursively bit by bit by using the vfn on
  the 'bit balance' to determine which value to match against."
  [vfn vbits]
  ((fn x [i vs]
    (if (= (length vs) 1)
      (get vs 0)
      (let [bits-at-i (map (fn [v] (get v i)) vs)
            bb (bit-balance bits-at-i)
            cmpv (vfn bb)]
        (x (inc i) (filter (fn [v] (= (get v i) cmpv)) vs)))))
  0 vbits))

(print
  (let [o2-rating (bits->val (filter-bits ones-win lines-bits))
        co2-rating (bits->val (filter-bits zeros-win lines-bits))]
    (* o2-rating co2-rating)))
