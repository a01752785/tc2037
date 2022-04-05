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