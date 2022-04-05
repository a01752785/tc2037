(ns parallelism)
(require '[clojure.math.numeric-tower :as math :refer [expt]])
;(time expresion)
;Procesadores logicos: (.availableProcessors (Runtime/getRuntime))
;Sp = T1 (1 procesador)/ (p procesadores)Tp
;Problem 1
(defn bits
  [x]
  (.bitCount (biginteger x)))
;Sequential version
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
;Sequential version
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
  [n width p]
  (map #(concat % [width])
       (partition 2
                  1
                  (concat (range 1 n (quot n p))
                          [(inc n)]))))
(defn compute-pi-par
  [n]
  (let [width (/ 1 n)]
    (->> (.availableProcessors (Runtime/getRuntime))
         (compute-pi-ranges n width)
         (pmap pi-range)
         (reduce +')
         (*' width))))
;Time tests
(def n 100000000)
(time (compute-pi n))
(time (compute-pi-par n))
;Problem 3

(defn palindrome?
  [s]
  (loop [i 0
         j (dec (count s))
         palindrome true]
    (if (>= i j)
      palindrome
      (recur
        (inc i)
        (dec j)
        (and palindrome (= (nth s i) (nth s j)))))))

(defn bin-hex-palindrome?
  [num]
  (and (palindrome? (Integer/toBinaryString num))
       (palindrome? (Integer/toHexString num))))

;Sequential version
(defn bin-hex-palindromes
  [n]
  (loop [num 0
         counter 0]
    (if (> num (expt 2 n))
      counter
      (recur
        (inc num)
        (#(if (bin-hex-palindrome? num)
            (inc %)
            %) counter)))))

;Parallel version
(defn compute-ranges-bin-hex
  [n p]
  (partition 2
             1
             (concat (range 0 n (quot n p))
                     [(inc n)])))

(defn bin-hex-palindromes-range
  [[start end]]
  (loop [num start
         counter 0]
    (if (= num end)
      counter
      (recur
        (inc num)
        (#(if (bin-hex-palindrome? num)
            (inc %)
            %) counter)))))

(defn bin-hex-palindromes-par
  [n]
  (->> (.availableProcessors (Runtime/getRuntime))
       (compute-ranges-bin-hex (expt 2 n))
       (pmap bin-hex-palindromes-range)
       (reduce +)))

;Time tests
(time (bin-hex-palindromes 24))
(time (bin-hex-palindromes-par 24))
