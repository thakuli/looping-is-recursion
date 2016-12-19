(ns looping-is-recursion)

(defn power [base exp]
  (let [ helper 
        (fn [base exp acc] 
          (if (= exp 0) 
            acc
            (recur base (dec exp) (* base acc)))) ]
        (helper base exp 1)))


(defn last-element [a-seq]
  (let [ helper 
        (fn [a-seq last] 
          (if (empty? a-seq) 
            last
            (recur (rest a-seq) (first a-seq)))) ]
    (helper a-seq (first a-seq))))


(defn seq= [seq1 seq2]
  (let [ helper 
        (fn [ seq1 seq2] 
          (cond
            (and (empty? seq1) (not (empty? seq2))) false
            (and (empty? seq2) (not (empty? seq1))) false
            (and (empty? seq2) (empty? seq1)) true
            (not (= (first seq1) (first seq2))) false
            
            :else (recur (rest seq1) (rest seq2)))) ]
        (helper seq1 seq2)))

(defn find-first-index [pred a-seq]
  (loop [ pred? pred
          seq a-seq
          index 0 ]
    (if (empty? seq)
      nil
      (do (if (pred? (first seq))
            index
            (recur pred? (rest seq) (inc index)))))))

(defn avg [a-seq]
  (loop [ seq a-seq
          sum 0
          items 0 ]
    (if (empty? seq)
      (/ sum items)
      (recur (rest seq) (+ sum (first seq)) (inc items)))))


(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (loop [ seq a-seq
          my-set (set []) ]
    (if (empty? seq)
      my-set
      (do (recur (rest seq) (toggle my-set (first seq)))))))

(defn fast-fibo [n]
  (loop [prev -1
	result 1
	sum 0
	counter 0 ]
	(if (= counter (inc n))
		result
		(let [ sum ( + result prev)
		       prev result
			result sum ]
			(recur prev result sum (inc counter))))))

(defn cut-at-repetition [a-seq]
  [":("])

