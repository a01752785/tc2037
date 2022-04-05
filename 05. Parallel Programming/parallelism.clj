(ns parallelism)
;(time expresion)
;Procesadores logicos: (.availableProcessors (Runtime/getRuntime))
;Sp = T1 (1 procesador)/ (p procesadores)Tp
;Problem 1
(defn bits
  [x]
  (.bitCount (biginteger x)))
;Sequencial version
(defn fact-seq
  [n]
  (bits
    (loop [i 1
           r 1]
      (if (> i n)
        r
        (recur (inc i) (*' i r))))))
;Parallel version
(defn fact-range
  [[start end]]
  (loop [i start
         r 1]
    (if (= i end)
      r
      (recur (inc i) (*' i r)))))
(defn compute-ranges
  [n p]
  (partition 2
             1
             (concat (range 1 n (quot n p))
                     [(inc n)])))
(defn fact-par
  [n]
  (->> (.availableProcessors (Runtime/getRuntime))
       (compute-ranges n)
       (pmap fact-range)
       (reduce *')
       (bits)))
;Time tests
(def n 300000)
(time (fact-seq n))
(time (fact-par n))
;Sp = 27.5402909/2.5358177 = 10.860516866019

;Problem 2
;Sequencial version
(defn compute-pi
  [n]
  (let [width (/ 1 n)]
    (loop [i 0
           sum 0]
      (if (>= i n)
        (* width sum)
        (let [mid (* width (+ i 0.5))
              height (/ 4 (+ 1 (* mid mid)))]
          (recur (inc i) (+ sum height)))))))
;Parallel version
(defn pi-range
  [[start end width]]
  (loop [i start
         sum 0]
    (if (= i end)
      sum
      (let [mid (* width (+ i 0.5))
            height (/ 4 (+ 1 (* mid mid)))]
        (recur (inc i) (+ sum height))))))
(defn compute-pi-ranges
  [n p]
  (map concat (partition 2
             1
             (concat (range 0 n (quot n p))
                     [(inc n)])) (repeat 5 (/ 1 n))))
(defn compute-pi-par
  [n]
  (let [width (/ 1 n)]
    (->> (.availableProcessors (Runtime/getRuntime))
         (compute-pi-ranges n)
         (pmap pi-range)
         (reduce +')
         (*' width))))
;Time tests
(def n 100000000)
(time (compute-pi n))
(time (compute-pi-par n))