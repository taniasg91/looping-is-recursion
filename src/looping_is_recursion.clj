(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc base exp]
                 (if (zero? exp)
                   acc
                   (recur (* acc base) base (dec exp))))]
    (helper 1 base exp)))

(defn last-element [a-seq]
  (let [helper (fn [acc b-seq]
                 (if (empty? b-seq)
                 acc
                 (recur (first b-seq) (rest b-seq))))]
    (helper nil a-seq)))

(defn seq= [seq1 seq2]
  (cond
    (and (empty? seq1) (empty? seq2)) true
    (or (empty? seq1) (empty? seq2)) false
    (not (= (first seq1) (first seq2))) false
    :else (recur (rest seq1) (rest seq2))))

(defn find-first-index [pred a-seq]
  (loop [acc 0
         loop-seq a-seq]
    (cond
      (empty? loop-seq) nil
      (= true (pred (first loop-seq))) acc
      :else (recur (inc acc) (rest loop-seq)))))

(defn avg [a-seq]
  (loop [sum 0
         count 0
         ns a-seq]
    (if (empty? ns)
      (/ sum count)
      (recur (+ sum (first ns))
             (inc count)
             (rest ns)))))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (loop [parityset #{}
         b-seq a-seq]
    (if (empty? b-seq)
      parityset
      (recur (toggle parityset (first b-seq)) (rest b-seq)))))

(defn fast-fibo [n]
  (loop [fn 1
         fn-1 0
         current 0]
    (if (>= current n) fn-1
      (recur (+ fn fn-1) fn (+ current 1)))))

(defn cut-at-repetition [a-seq]
  (loop [non-rep-set #{}
         result []
         b-seq a-seq]
    (cond
      (empty? b-seq) result
      (contains? non-rep-set (first b-seq)) result
      :else (recur (conj non-rep-set (first b-seq)) (conj result (first b-seq)) (rest b-seq)))))

