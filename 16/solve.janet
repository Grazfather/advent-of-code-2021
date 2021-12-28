(def packet (as-> (file/open "input") x
                  (file/read x :all)
                  (string/trim x)))

(def hex-map {"0" "0000" "1" "0001" "2" "0010" "3" "0011"
              "4" "0100" "5" "0101" "6" "0110" "7" "0111"
              "8" "1000" "9" "1001" "A" "1010" "B" "1011"
              "C" "1100" "D" "1101" "E" "1110" "F" "1111"})
(defn hexnybble->bits [hn]
  (hex-map hn))

(defn hexstring->bits [hs]
  (as-> hs x
       (map string/from-bytes x)
       (map hexnybble->bits x)
       (string/join x "")))

(defn bits->int-rec [bs v]
  (if-not (empty? bs)
    (bits->int-rec (drop 1 bs) (+ (* 2 v) (- (get bs 0) 0x30)))
    v))

(defn bits->int [bs]
  (bits->int-rec bs 0))

(var parse-packet nil)

(defn parse-literal-rec [v parts]
  (let [part (first parts)
        prefix (->> part (take 1) bits->int)
        parts (drop 1 parts)
        v (+ (* v 16) (bits->int (drop 1 part)))]
    (if (= prefix 1)
      (parse-literal-rec v parts)
      [v (string/join parts "")])))

(defn parse-literal [ver t bs]
  (let [parts (partition 5 bs)
        [value rest] (parse-literal-rec 0 parts)]
    [{:ver ver :type t :literal value} rest]))

(defn parse-operator [ver t bs]
  (let [lt (->> bs (take 1) bits->int)
        bs (drop 1 bs)]
    (if (= lt 0)
      # Type 0: Parse a specific count of bits
      (let [l (->> bs (take 15) bits->int)
            bs (drop 15 bs)
            subpacketbits (take l bs)
            bs (drop l bs)]
        (var subpacketbits subpacketbits)
        (def packets @[])
        (while (not (empty? subpacketbits))
          (let [[packet leftovers] (parse-packet subpacketbits)]
            (array/push packets packet)
            (set subpacketbits leftovers)))
        [{ :ver ver :type t :ltype 0 :len l :contains packets } bs])
      # Type 1: Parse a speific number of packets
      (let [np (->> bs (take 11) bits->int)
            bs (drop 11 bs)]
        (def packets @[])
        (var bs bs)
        (for _ 0 np
          (let [[packet leftovers] (parse-packet bs)]
            (array/push packets packet)
            (set bs leftovers)))
        [{ :ver ver :type t :ltype 1 :np np :contains packets } bs]))))

(varfn parse-packet [pb]
  (let [ver (->> pb (take 3) bits->int)
        pb (drop 3 pb)
        t (->> pb (take 3) bits->int)
        pb (drop 3 pb)]
    (if (= 4 t)
      (parse-literal ver t pb)
      (parse-operator ver t pb))))

# Part 1: Count the version sum of all packets
(var count-version-sum nil)
(defn count-version-sum-rec [packet v]
  (let [ver (packet :ver)
        inner (packet :contains)]
    (if-not (nil? inner)
      (+ v ver (sum (map |(count-version-sum $) inner)))
      (+ v ver))))

(varfn count-version-sum [packet]
  (count-version-sum-rec packet 0))

# Part 2: Evaluate the expression from the packet
(defn evaluate-expression [packet]
  (let [op (packet :type)
        subpackets (packet :contains)
        subeval (when (not (nil? subpackets))
                  (map evaluate-expression subpackets))]
    (cond
      (= op 0) (+ ;subeval)
      (= op 1) (* ;subeval)
      (= op 2) (min ;subeval)
      (= op 3) (max ;subeval)
      (= op 4) (packet :literal)
      (= op 5) (if (> (first subeval) (get subeval 1)) 1 0)
      (= op 6) (if (< (first subeval) (get subeval 1)) 1 0)
      (= op 7) (if (= (first subeval) (get subeval 1)) 1 0))))

(print (count-version-sum (first (parse-packet (hexstring->bits packet)))))
(print (evaluate-expression (first (parse-packet (hexstring->bits packet)))))
