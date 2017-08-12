(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc exp]
                 (cond (zero? exp)
                       acc
                       (zero? base)
                       0
                       :else
                       (recur (* acc base) (dec exp))))]
    (helper 1 exp)))

(defn last-element [a-seq]
  (if (empty? a-seq)
    nil
    (let [helper (fn [a-seq]
                   (if (== 1 (count a-seq))
                     (first a-seq)
                     (recur (rest a-seq))))]
          (helper a-seq))))

(defn seq= [seq1 seq2]
  (cond (and (empty? seq1) (empty? seq2))
        true
        (or (not (== (count seq1) (count seq2))) (empty? seq1) (empty? seq2))
        false
        :else
        (let [helper (fn [seg1 seg2]
                       (if (and (empty? seg1) (empty? seg2))
                         true
                         (if (= (first seg1) (first seg2))
                           (recur (rest seg1) (rest seg2))
                           false)))]
          (helper seq1 seq2))))

(defn find-first-index [pred a-seq]
  (if (empty? a-seq)
    nil
  (loop [acc 0]
    (cond (pred (get a-seq acc))
            acc
          (== acc (dec (count a-seq)))
            nil
          :else
            (recur (inc acc))))))

(defn avg [a-seq]
  (if (empty? a-seq)
        nil
      (loop [sum 0
             seq1 a-seq]
        (if (empty? seq1)
          (/ sum (count a-seq))
          (recur (+ sum (first seq1)) (rest seq1))))))

(defn parity [a-seq]
  (if (empty? a-seq)
    (set a-seq)
    (loop [setti #{}
           seq1 a-seq]
      (if (empty? seq1)
        setti
        (if (contains? setti (first seq1))
              (recur (disj setti (first seq1)) (rest seq1))
              (recur (conj setti (first seq1)) (rest seq1)))))))

(defn fast-fibo [n]
  (cond (== n 0)
        0
        (or (== n 2) (== n 1))
        1
        :else
  (loop [f-n 1
         f-n-1 0
         mones 1]
    (if (== n mones)
      f-n
      (recur (+ f-n f-n-1) f-n (inc mones))))))


(defn cut-at-repetition [a-seq]
  (if (empty? a-seq)
    a-seq
    (loop [seq1 []
           seq2 a-seq
           setti #{}]
      (cond (empty? seq2)
            seq1
            (contains? setti (first seq2))
            seq1
            :else
            (recur (conj seq1 (first seq2)) (rest seq2) (conj setti (first seq2)))))))
