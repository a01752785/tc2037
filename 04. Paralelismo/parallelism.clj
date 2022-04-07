(ns parallelism)

(defn bits
  [x]
  (.bitCount (biginteger x)))

(defn fact-sec
  [n]
  (bits
    (loop [i 1
         r 1]
    (if (> i n)
      r
      (recur (inc i)
             (*' i r))))))

(defn fact-range
  [[start end]]
  (loop [i start
         r 1]
    (if (= i end)
      r
      (recur (inc i)
             (*' i r)))))

(defn compute-ranges
  [n p]
  (partition 2
             1
             (concat (range 1 n (quot n p))
          [(inc n)])))

(compute-ranges 1000 5)

(defn fact-par
  [n]
  (->> (.availableProcessors (Runtime/getRuntime))
       (compute-ranges n)
       (pmap fact-range)
       (reduce *')
       bits))

(fact-par 1000)

(time (fact-sec 5))
(time (fact-sec 1000))
(time (fact-sec 10000))
(time (fact-sec 100000))

(time (fact-par 5))

(time (fact-sec 100000))
(time (fact-par 100000))

;;; T1 = 4496.2358
;;; T8 = 485.8042
;;; Sp = 9.25

; Problema 4

(defn create-random-data
  [n]
  (repeatedly n #(rand-int 1000)))

(apply <= (create-random-data 1000))

(defn insertion-sort
  [s]
  (loop [s s
         r ()]
    (if (empty? s)
      r
      (let [x (first s)
            [before after] (split-with #(< % x) r)]
        (recur (rest s)
               (concat before [x] after))))))

(insertion-sort [5 2 9 8 1 7 4 3])

(defn merge-algorithm
  [a b]
  (loop [a a
         b b
         r ()]
    (cond
      (empty? a) (concat (reverse r) b)
      (empty? b) (concat (reverse r) a)
      (< (first a) (first b)) (recur (rest a) b (cons (first a) r))
      :else (recur a (rest b) (cons (first b) r)))))

(merge-algorithm [1 2 5 7] [1 3 4 5 6 7 8 9])

(defn hybrid-sort-seq
  [s]
  (if (< (count s) 100)
    (insertion-sort s)
    (let [[left right] (split-at (quot (count s) 2) s)]
      (merge-algorithm
        (hybrid-sort-seq left)
        (hybrid-sort-seq right)))))

(defn hybrid-sort-par
  [s]
  (if (< (count s) 100)
    (insertion-sort s)
    (let [splitted (split-at (quot (count s) 2) s)]
      (apply merge-algorithm
        (pmap hybrid-sort-par splitted)))))

(def n 100000)
(def random-data (create-random-data n))
(apply <= random-data)
(apply <= (time (hybrid-sort-seq random-data)))
(apply <= (time (hybrid-sort-par random-data)))