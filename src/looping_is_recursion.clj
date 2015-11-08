(ns looping-is-recursion)

(defn power [base exp]
  (let [pwr (fn [acc base exp]
              (if (= exp 1)
                acc
                (recur (* acc base) base (- exp 1))))]
    (if (= exp 0)
      1
      (pwr base base exp))))

(defn last-element [a-seq]
  (if (>= 1 (count a-seq))
    (first a-seq)
    (recur (rest a-seq))))

(defn seq= [seq1 seq2]
  (cond
    (= 0 (count seq1) (count seq2)) true
    (not= (count seq1) (count seq2)) false
    (not= (first seq1) (first seq2)) false
    :else (recur (rest seq1) (rest seq2))))

(defn find-first-index [pred a-seq]
  (loop [i 0
         b-seq a-seq]
    (cond
      (zero? (count b-seq)) nil
      (pred (first b-seq)) i
      :else (recur (inc i) (rest b-seq)))))

(defn avg [a-seq]
  (loop [c 0
         sum 0
         b-seq a-seq]
    (if (empty? b-seq)
      (if (zero? c)
        nil
        (/ sum c))
      (recur (inc c) (+ sum (first b-seq)) (rest b-seq)))))

(defn parity [a-seq]
  (loop [a-set #{}
         b-seq a-seq]
    (if (empty? b-seq)
      a-set
      (let [elem (first b-seq)]
        (if (contains? a-set elem)
          (recur (disj a-set elem) (rest b-seq))
          (recur (conj a-set elem) (rest b-seq)))))))

(defn fast-fibo [n]
  (loop [fst 0
         sec 1
         iter 0]
    (if (= iter n )
      fst
      (recur sec (+ fst sec) (inc iter)))))

(defn cut-at-repetition [a-seq]
  (loop [found #{}
         collected []
         b-seq a-seq]
    (if (or (empty? b-seq) (contains? found (first b-seq)))
      collected
      (recur (conj found (first b-seq)) (conj collected (first b-seq)) (rest b-seq)))))

