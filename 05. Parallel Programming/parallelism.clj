;----------------------------------------------------------
; Problem Set #5: Parallel Programming
; Date: April 8, 2022.
; Authors:
;          A01770771 Luis Humberto Romero Pérez
;          A01777771 David Damián Galán
;----------------------------------------------------------
(ns parallelism)
(require '[clojure.math.numeric-tower :as math :refer [expt]])
;Sp = T1 (1 processors)/ Tp (p processors)
;All time tests were done using a computer with 12 logical CPU cores

;Problem 1
(defn bits
  "Function that counts the number of bits equal to 1 in the
  binary representation of any integer value x."
  [x]
  (.bitCount (biginteger x)))
;Sequential version
(defn fact-seq
  "Function that returns the number of bits equal to 1 from the
  binary result of the factorial of n, computed sequentialy."
  [n]
  (bits
    (loop [i 1
           r 1]
      (if (> i n)
        r
        (recur (inc i) (*' i r))))))
;Parallel version
(defn fact-range
  "Function that computes part of a factorial using a range of numbers,
  from start to end minus 1."
  [[start end]]
  (loop [i start
         r 1]
    (if (= i end)
      r
      (recur (inc i) (*' i r)))))

(defn compute-ranges
  "Function that returns a sequence of sequences representing the
  start and end of a range whose partial of factorial will be
  computed."
  [n p]
  (partition 2
             1
             (concat (range 1 n (quot n p))
                     [(inc n)])))

(defn fact-par
  "Function that returns the number of bits equal to 1 from the
  binary result of the factorial of n, computed using parallelism."
  [n]
  (->> (.availableProcessors (Runtime/getRuntime))
       (compute-ranges n)
       (pmap fact-range)
       (reduce *')
       (bits)))
;Time tests
(time (fact-seq 300000))
(time (fact-par 300000))
;T1 = 28311.8296 | 27606.1349 | 27807.1326 | 28099.6573 | 28655.5794
;Tp = 3311.3707 | 2567.9747 | 2641.5359 | 2690.1073 | 2806.0463
;p = 12
;Sp = (8.54988 + 10.75015 + 10.52688 + 10.44555 + 10.21208)/5
;Sp = 10.096908

;Problem 2
;Sequential version
(defn compute-pi
  "Function that computes an approximation of Pi using numerical
  integration (midpoint rectangle rule), sequentialy computed."
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
  "Function that computes part of a numerical integration using the
   midpoint rectangle rule, having start and end as its limits, and
   width as the width of each rectangule in the approximation."
  [[start end width]]
  (loop [i start
         sum 0]
    (if (= i end)
      sum
      (let [mid (* width (+ i 0.5))
            height (/ 4 (+ 1 (* mid mid)))]
        (recur (inc i) (+ sum height))))))

(defn compute-pi-ranges
  "Function that returns a sequence of sequences representing the
  start and end of a range whose partial numeric integration will
  be computed."
  [n width p]
  (map #(concat % [width])
       (partition 2
                  1
                  (concat (range 1 n (quot n p))
                          [(inc n)]))))

(defn compute-pi-par
  "Function that computes an approximation of Pi using numerical
  integration (midpoint rectangle rule), computed using parallelism."
  [n]
  (let [width (/ 1 n)]
    (->> (.availableProcessors (Runtime/getRuntime))
         (compute-pi-ranges n width)
         (pmap pi-range)
         (reduce +')
         (*' width))))
;Time tests
(time (compute-pi 100050000))
(time (compute-pi-par 100050000))
;T1 = 35204.2108 | 35035.2756 | 34789.0955 | 33918.3806 | 35053.2578
;Tp = 7751.991 | 7778.7415 | 7955.5064 | 8108.4254 | 7714.8875
;p = 12
;Sp = (4.54131 + 4.50397 + 4.37295 + 4.1831 + 4.54358)/5
;Sp = 4.428982

;Problem 3
(defn palindrome?
  "Function that returns true if the string given as an input
  is a palindrome, returns false otherwise."
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
  "Function that returns true if both the binary and hexadecimal form
  of the number given are palindromes, returns false otherwise."
  [num]
  (and (palindrome? (Integer/toBinaryString num))
       (palindrome? (Integer/toHexString num))))
;Sequential version
(defn bin-hex-palindromes
  "Function that counts the amount of bin-hex-palindromes from 0 to
  2^n., computed sequentialy."
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
  "Function that returns a sequence of sequences representing the
  start and end of a range whose amount of bin-hex-palindromes will
  be computed."
  [n p]
  (partition 2
             1
             (concat (range 0 n (quot n p))
                     [(inc n)])))

(defn bin-hex-palindromes-range
  "Function that computes the amount of bin-hex-palindromes in the
  range given from start to end minus 1."
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
  "Function that counts the amount of bin-hex-palindromes from 0 to
  2^n., computed using parallelism."
  [n]
  (->> (.availableProcessors (Runtime/getRuntime))
       (compute-ranges-bin-hex (expt 2 n))
       (pmap bin-hex-palindromes-range)
       (reduce +)))
;Time tests
(time (bin-hex-palindromes 26))
(time (bin-hex-palindromes-par 26))
;T1 = 17442.5225 | 18267.9037 | 17951.5702 | 18111.6246 | 17810.966
;Tp = 1282.3458 | 1328.0897 | 1225.8577 | 1222.2538 | 1235.1598
;p = 12
;Sp = (13.60204 + 13.75502 + 14.64408 + 14.81821 + 14.41996)/5
;Sp = 14.247862

;Problem 4
(defn create-random-data
  "Function that creates a sequence of n random numbers from 1 to 999."
  [n]
  (repeatedly n #(rand-int 1000)))

(defn insertion-sort
  "Function that implements the insertion sort algorithm in the
  given sequence s."
  [s]
  (loop [s s
         r ()]
    (if (empty? s)
      r
      (let [x (first s)
            [before after] (split-with #(< % x) r)]
        (recur (rest s)
               (concat before [x] after))))))

(defn merge-algorithm
  "Function that implements the merge algorithm in the pair of
  sequences given (a and b)."
  [a b]
  (loop [a a
         b b
         r ()]
    (cond
      (empty? a) (concat (reverse r) b)
      (empty? b) (concat (reverse r) a)
      (< (first a) (first b)) (recur (rest a) b (cons (first a) r))
      :else (recur a (rest b) (cons (first b) r)))))
;Sequential version
(defn hybrid-sort-seq
  "Function that implements a hybrid sorting algorithm, which uses
  insertion sort if the sequence s has less than 100 elements,
  otherwise splits the sequence in two, ordering them recursively
  and joining them using the merge algorithm. Works sequentialy."
  [s]
  (if (< (count s) 100)
    (insertion-sort s)
    (let [[a b] (split-at (quot (count s) 2) s)]
      (merge-algorithm
        (hybrid-sort-seq a)
        (hybrid-sort-seq b)))))
;Parallel version
(defn hybrid-sort-par
  "Function that implements a hybrid sorting algorithm, which uses
  insertion sort if the sequence s has less than 100 elements,
  otherwise splits the sequence in two, ordering them recursively
  and joining them using the merge algorithm. Works using parallelism."
  [s]
  (if (< (count s) 100)
    (insertion-sort s)
    (let [splitted (split-at (quot (count s) 2) s)]
      (apply merge-algorithm
             (pmap hybrid-sort-par splitted)))))
;Time tests
(def random-data (create-random-data 1000000))
(apply <= random-data)
(apply <= (time (hybrid-sort-seq random-data)))
(apply <= (time (hybrid-sort-par random-data)))
;T1 = 14528.1367 | 13548.3301 | 13465.0707 | 13532.5238 | 13207.1166
;Tp = 5229.8258 | 5279.476 | 6218.7884 | 5477.1396 | 5315.4084
;p = 12
;Sp = (2.77793 + 2.56622 + 2.16522 + 2.47072 + 2.48468)/5
;Sp = 2.492954