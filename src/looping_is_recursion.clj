(ns looping-is-recursion)

(defn power [base exp]
  (let [ helper (fn
                  [acc base exp]
                  (cond
                   (zero? exp) 1
                   (= exp 1) acc
                   :else (recur (* acc base) base (dec exp))))
         ]
    (helper base base exp)))

(first '())

(defn last-element [a-seq]
  (if (<= (count a-seq) 1)
    (first a-seq)
    (recur (rest a-seq))))


(defn seq= [seq1 seq2]
  (cond
   (and (empty? seq1) (empty? seq2)) true
   (or (empty? seq1) (empty? seq2)) false
   (= (first seq1) (first seq2)) (recur (rest seq1) (rest seq2))
   :else false))

(defn find-first-index [pred a-seq]
  (loop [i 0
         loop-seq a-seq]
    (cond
     (pred (first loop-seq)) i
     (empty? (rest loop-seq)) nil
     :else (recur (inc i) (rest loop-seq)))))

(defn avg [a-seq]
  (loop [acc 0
         i 0
         loop-seq a-seq]
    (if (empty? loop-seq)
      (/ acc i)
      (recur (+ acc (first loop-seq)) (inc i) (rest loop-seq)))))


(defn parity [a-seq]
  ":(")

(defn fast-fibo [n]
  ":(")

(defn cut-at-repetition [a-seq]
  [":("])

