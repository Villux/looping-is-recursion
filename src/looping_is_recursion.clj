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
     (empty? loop-seq) nil
     (pred (first loop-seq)) i
     :else (recur (inc i) (rest loop-seq)))))

(defn avg [a-seq]
  (loop [acc 0
         i 0
         loop-seq a-seq]
    (if (empty? loop-seq)
      (/ acc i)
      (recur (+ acc (first loop-seq)) (inc i) (rest loop-seq)))))


(defn parity [a-seq]
  (let [odd-seq (filter #(not (= 0 (mod (get % 1) 2))) (frequencies a-seq))]
    (set (keys odd-seq))))


(defn fast-fibo [n]
  (loop [fib1 0
         fib2 1
         n n]
    (if (= n 0)
      fib1
     (recur fib2 (+ fib1 fib2) (dec n)))))

(conj [1 2 3] 2)



(defn cut-at-repetition [a-seq]
  (loop [a-seq a-seq result []]
    (if
      (or (empty? a-seq) (some #(= % (first a-seq)) result))
          result
          (recur (rest a-seq) (conj result (first a-seq))))))


