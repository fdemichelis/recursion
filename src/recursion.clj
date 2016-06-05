(ns recursion)

(defn product [coll]
  (if (empty? coll) 1
    (* (first coll) (product (rest coll)))))


(defn singleton? [coll]
  (if (and (not (empty? coll))
           (empty? (rest coll))) true false))



(defn my-last [coll]
  (if (empty? (rest coll)) (first coll) (my-last (rest coll))))


(defn max-element [a-seq]
  (if (empty? a-seq) nil
    (if (singleton? a-seq) (first a-seq)
      (max (first a-seq) (max-element (rest a-seq))))))


(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2)) seq-1 seq-2))


(defn longest-sequence [a-seq]
  (if (empty? a-seq) nil
    (if (singleton? a-seq) (first a-seq)
      (seq-max (first a-seq) (longest-sequence (rest a-seq))))))


(defn my-filter [pred? a-seq]
  (if (empty? a-seq) a-seq
    (if (pred? (first a-seq)) (cons (first a-seq) (my-filter pred? (rest a-seq)))
      (my-filter pred? (rest a-seq)))))

(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq) false
   (= (first a-seq) elem) true
   :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (cond
   (empty? a-seq) a-seq
   (pred? (first a-seq)) (cons (first a-seq) (my-take-while pred? (rest a-seq)))
   :else (empty a-seq)))

(defn my-drop-while [pred? a-seq]
(cond
   (empty? a-seq) a-seq
   (pred? (first a-seq)) (my-drop-while pred? (rest a-seq))
   :else a-seq))

(defn seq= [a-seq b-seq]
  (cond
   (and (empty? a-seq) (empty? b-seq)) true
   (or (empty? a-seq) (empty? b-seq)) false
   (= (first a-seq) (first b-seq)) (seq= (rest a-seq) (rest b-seq))
   :else false))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2)) '()
   (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (cond
   (= 0 k) 1
   (= 0 (mod k 2)) (power (* n n) (/ k 2))
   :else (* n (power (* n n) (/ (dec k) 2)))))

(defn fib [n]
  (if (< n 2) n
    (+ (fib (dec n)) (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (>= 0 how-many-times) '()
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (<= up-to 0) '()
    (cons (dec up-to) (my-range (dec up-to)))))


(defn tails [a-seq]
  (if (empty? a-seq) (seq ['()])
    (cons a-seq (tails (rest a-seq)))))

(defn inits [a-seq]
  (reverse (map reverse (tails (reverse a-seq)))))


(defn rotations [a-seq]
  (if (empty? a-seq) (tails a-seq)
  (rest (my-map concat (tails a-seq) (inits a-seq)))))

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq) freqs
    (my-frequencies-helper (assoc freqs (first a-seq) (count (filter (fn [v] (= v (first a-seq))) a-seq))) (filter (fn [v] (not= v (first a-seq))) a-seq))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))


(defn un-frequencies [a-map]
  (if (empty? a-map) '()
  (concat (my-repeat (second (first a-map)) (first (first a-map))) (un-frequencies (rest a-map)))))



(defn my-take [n coll]
    (if (or (empty? coll) (= 0 n)) '()
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
(if (or (empty? coll) (= 0 n)) coll
    (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [s (int (/ (count a-seq) 2))]
    [(my-take s a-seq), (my-drop s a-seq)]))

(defn seq-merge [a-seq b-seq]
  (cond
    (empty? a-seq) b-seq
    (empty? b-seq) a-seq
    (< (first a-seq) (first b-seq)) (cons (first a-seq) (seq-merge (rest a-seq) b-seq))
    :else (cons (first b-seq) (seq-merge a-seq (rest b-seq)))))

(defn merge-sort [a-seq]
  (let [[h1 h2] (halve a-seq)]
    (if (empty? h1) h2
      (seq-merge (merge-sort h1) (merge-sort h2)))))

(defn split-into-monotonics [a-seq]
  (if (empty? a-seq) nil
  (let [l1 (last (take-while (fn [l] (if (empty? l) true (apply < l))) (inits a-seq)))
       l2 (last (take-while (fn [l] (if (empty? l) true (apply > l))) (inits a-seq)))]
  (if (>= (count l1) (count l2))
    (cons l1 (split-into-monotonics (drop (count l1) a-seq)))
    (cons l2 (split-into-monotonics (drop (count l2) a-seq)))))))


(defn permutations [a-set]
  (if (< (count a-set) 3) (rotations a-set)
    (apply concat (map (fn [v]
                         (let [p (permutations (rest v))]
                         (map cons (repeat (count p) (first v)) p))) (rotations a-set)))))

(defn powerset [a-set]
  (if (empty? a-set) #{#{}}
     (set ((fn [v] (clojure.set/union (apply clojure.set/union (map (fn [x] (powerset (disj v x))) v)) #{v})) (set a-set)))))


