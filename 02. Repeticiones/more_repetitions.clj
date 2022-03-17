;----------------------------------------------------------
; Problem Set #3: More Repetitions
; Date: March 14, 2022.
; Authors:
;          A01752789 Luis Humberto Romero Pérez
;          A01752785 David Damián Galán
;----------------------------------------------------------
(ns more_repetitions
  (:require [clojure.test :refer [deftest is run-tests]]))
(require '[clojure.math.numeric-tower :refer [abs]])
;Problem 1
(defn expand
  "Function that takes a sequence s and returns a list
  where the first element appears one time, the second
  element appears two times and so on."
  [s]
  (loop [i 0
         result ()]
    (if (>= i (count s))
      result
      (recur (inc i)
             (concat result (repeat (+ i 1) (nth s i)))))))
(deftest test-expand
  (is (= () (expand ())))
  (is (= '(a) (expand '(a))))
  (is (= '(1 2 2 3 3 3 4 4 4 4) (expand '(1 2 3 4))))
  (is (= '(a b b c c c d d d d e e e e e)
         (expand '(a b c d e)))))

;Problem 2
;Usando recursion
;(defn insert
;  [n s]
;  (cond
;    (empty? s) (list n)
;    (<= n (first s)) (cons n s)
;    :else (cons (first s)
;                (insert n (rest s)))))
;Usando más API's
(defn insert
  "Function that returns a list with the elements of sequence s,
   inserting n in the correct place."
  [n s]
  (let [[inicio final] (split-with #(< % n) s)]
    (concat inicio (list n) final)))
(deftest test-insert
  (is (= '(14) (insert 14 ())))
  (is (= '(4 5 6 7 8) (insert 4 '(5 6 7 8))))
  (is (= '(1 3 5 6 7 9 16) (insert 5 '(1 3 6 7 9 16))))
  (is (= '(1 5 6 10) (insert 10 '(1 5 6)))))

;Problem 3
;Recursion O(N^2)
(defn insertion-sort
  "Performs insertion sort algorithm on sequence s."
  [s]
  (if (empty? s)
    ()
    (insert (first s) (insertion-sort (rest s)))))
(deftest test-insertion-sort
  (is (= () (insertion-sort ())))
  (is (= '(0 1 3 3 4 6 7 8 9)
         (insertion-sort '(4 3 6 8 3 0 9 1 7))))
  (is (= '(1 2 3 4 5 6) (insertion-sort '(1 2 3 4 5 6))))
  (is (= '(1 5 5 5 5 5 5) (insertion-sort '(5 5 5 1 5 5 5)))))

;Problema 4
(defn rotate-left
  "Rotates a sequence to the left n places. If n is negative,
  performs a rotation to the right."
  [n s]
  (if (empty? s)
    ()
    (let [num (rem n (count s))
          num2 (rem (+ num (count s)) (count s))]
      (concat (last (split-at num2 s)) (first (split-at num2 s))))))
(deftest test-rotate-left
  (is (= () (rotate-left 5 ())))
  (is (= '(a b c d e f g) (rotate-left 0 '(a b c d e f g))))
  (is (= '(b c d e f g a) (rotate-left 1 '(a b c d e f g))))
  (is (= '(g a b c d e f) (rotate-left -1 '(a b c d e f g))))
  (is (= '(d e f g a b c) (rotate-left 3 '(a b c d e f g))))
  (is (= '(e f g a b c d) (rotate-left -3 '(a b c d e f g))))
  (is (= '(a b c d e f g) (rotate-left 7 '(a b c d e f g))))
  (is (= '(a b c d e f g) (rotate-left -7 '(a b c d e f g))))
  (is (= '(b c d e f g a) (rotate-left 8 '(a b c d e f g))))
  (is (= '(g a b c d e f) (rotate-left -8 '(a b c d e f g))))
  (is (= '(d e f g a b c) (rotate-left 45 '(a b c d e f g))))
  (is (= '(e f g a b c d) (rotate-left -45 '(a b c d e f g)))))

;Problem 5
(defn binary
  "Returns a sequence containing the binary representation of
  the decimal number n"
  [n]
  (if (zero? n)
    []
    (conj (binary (quot n 2)) (rem n 2))))
(deftest test-binary
  (is (= () (binary 0)))
  (is (= '(1 1 1 1 0) (binary 30)))
  (is (= '(1 0 1 1 0 0 0 0 0 1 0 0 0 0 1 1) (binary 45123))))

;Problem 6
(defn prime?
  "Auxiliar function that returns true if n is prime, false otherwise."
  [n]
  (loop [i 1
         div 0]
    (if (> i n)
      (= div 2)
      (recur (inc i) (if (= (rem n i) 0)
                       (inc div)
                       div)))))
(defn prime-aux
  "Auxiliar function that returns a sequence with the prime factors of n checking prime numbers greater or equal to i."
  [n i]
  (if (= n 1)
    ()
    (if (and (= (rem n i) 0) (prime? i))
      (cons i (prime-aux (/ n i) i))
      (prime-aux n (inc i)))))
(defn prime-factors
  "Returns a sequence with the prime factors of n."
  [n]
  (prime-aux n 2))
(deftest test-prime-factors
  (is (= () (prime-factors 1)))
  (is (= '(2 3) (prime-factors 6)))
  (is (= '(2 2 2 2 2 3) (prime-factors 96)))
  (is (= '(97) (prime-factors 97)))
  (is (= '(2 3 3 37) (prime-factors 666))))

;Problem 7
(defn gcd
  "Returns the greater common divisor of two numbers a and b."
  [a b]
  (if (= (rem a b) 0)
    b
    (gcd b (rem a b))))
(deftest test-gcd
  (is (= 1 (gcd 13 7919)))
  (is (= 4 (gcd 20 16)))
  (is (= 6 (gcd 54 24)))
  (is (= 7 (gcd 6307 1995)))
  (is (= 12 (gcd 48 180)))
  (is (= 14 (gcd 42 56))))

;Problem 8
(defn insert-at
  "Auxiliar function that receives an element, a sequence and a position
  and inserts the element in the selected position."
  n s i]
  (concat (first (split-at i s)) (list n) (last (split-at i s))))
(defn insert-everywhere
  "Returns a sequence with all the possible sequences made by inserting element x in s in all different positions."
  [x s]
  (map #(insert-at x s %) (range (+ (count s) 1))))
(deftest test-insert-everywhere
  (is (= '((1)) (insert-everywhere 1 ())))
  (is (= '((1 a) (a 1)) (insert-everywhere 1 '(a))))
  (is (= '((1 a b c) (a 1 b c) (a b 1 c) (a b c 1))
         (insert-everywhere 1 '(a b c))))
  (is (= '((1 a b c d e)
           (a 1 b c d e)
           (a b 1 c d e)
           (a b c 1 d e)
           (a b c d 1 e)
           (a b c d e 1))
         (insert-everywhere 1 '(a b c d e))))
  (is (= '((x 1 2 3 4 5 6 7 8 9 10)
           (1 x 2 3 4 5 6 7 8 9 10)
           (1 2 x 3 4 5 6 7 8 9 10)
           (1 2 3 x 4 5 6 7 8 9 10)
           (1 2 3 4 x 5 6 7 8 9 10)
           (1 2 3 4 5 x 6 7 8 9 10)
           (1 2 3 4 5 6 x 7 8 9 10)
           (1 2 3 4 5 6 7 x 8 9 10)
           (1 2 3 4 5 6 7 8 x 9 10)
           (1 2 3 4 5 6 7 8 9 x 10)
           (1 2 3 4 5 6 7 8 9 10 x))
         (insert-everywhere 'x '(1 2 3 4 5 6 7 8 9 10)))))

;Problem 9
(defn contains-all-digits?
  "Returns true if a number contains all decimal digits, false otherwise."
  [n]
  (loop [digits #{}
         n (abs n)]
    (if (= n 0)
      (= (count digits) 10)
      (recur (conj digits (rem n 10)) (quot n 10)))))
(deftest test-contains-all-digits?
  (is (contains-all-digits? 1023456789))
  (is (contains-all-digits? 5897230146))
  (is (contains-all-digits? 10123485679))
  (is (contains-all-digits?
        1223334444555566666677777778888888889999999990))
  (is (not (contains-all-digits? 1236)))
  (is (not (contains-all-digits? 1112223334455)))
  (is (not (contains-all-digits? -587230462413578)))
  (is (not (contains-all-digits?
             -122333444455556666667777777888888888999999999))))

;Problem 10
(defn pack
  "A function that takes a sequence s. If s contains consecutive
  repeated elements they are placed and returned in separate sublists."
  [s]
  (partition-by (fn [x] x) s))
(deftest test-pack
  (is (= () (pack ())))
  (is (= '((a a a a) (b) (c c) (a a) (d) (e e e e))
         (pack '(a a a a b c c a a d e e e e))))
  (is (= '((1) (2) (3) (4) (5)) (pack '(1 2 3 4 5))))
  (is (= '((9 9 9 9 9 9 9 9 9)) (pack '(9 9 9 9 9 9 9 9 9)))))

;Problem 11
(defn compress
  "Function that takes a sequence s. It returns a list with the
  same elements but repeated consecutive elements are
  replaced with a single instance. Conserving the order
  of the original elements."
  [s]
  (map first (pack s)))
(deftest test-compress
  (is (= () (compress ())))
  (is (= '(a b c d) (compress '(a b c d))))
  (is (= '(a b c a d e)
         (compress '(a a a a b c c a a d e e e e))))
  (is (= '(a) (compress '(a a a a a a a a a a)))))

;Problem 12
(defn encode
  "Function that takes a sequence s and returns a list where
  consecutive repeated elements are encoded as vectors [n e],
  where n is the number of repetitions of the element e."
  [s]
  (let [elements (pack s)]
    (map (fn [e] (vector (count e) (first e))) elements)))
(deftest test-encode
  (is (= () (encode ())))
  (is (= '([4 a] [1 b] [2 c] [2 a] [1 d] [4 e])
         (encode '(a a a a b c c a a d e e e e))))
  (is (= '([1 1] [1 2] [1 3] [1 4] [1 5])
         (encode '(1 2 3 4 5))))
  (is (= '([9 9]) (encode '(9 9 9 9 9 9 9 9 9)))))

;Problem 13
(defn encode-modified
  "Function that takes a sequence s. It works the same as
  the encode function, but if an element is single it
  is simply copied into the result list. Only elements
  with actual duplicates are converted to [n e] vectors."
  [s]
  (let [elements (pack s)]
    (map (fn [e]
           (if (= (count e) 1)
             (first e)
             (vector (count e) (first e))))
         elements)))
(deftest test-encode-modified
  (is (= () (encode-modified ())))
  (is (= '([4 a] b [2 c] [2 a] d [4 e])
         (encode-modified '(a a a a b c c a a d e e e e))))
  (is (= '(1 2 3 4 5) (encode-modified '(1 2 3 4 5))))
  (is (= '([9 9]) (encode-modified '(9 9 9 9 9 9 9 9 9)))))

;Problem 14
(defn decode
  "Function that takes as its input an encoded sequence s that
  has the same structure as the resulting list from the function
  encode-modified. It returns the decoded version of s."
  [s]
  (mapcat (fn [e]
            (if (vector? e)
              (repeat (first e) (last e))
              (list e))) s))
(deftest test-decode
  (is (= () (decode ())))
  (is (= '(a a a a b c c a a d e e e e)
         (decode '([4 a] b [2 c] [2 a] d [4 e]))))
  (is (= '(1 2 3 4 5) (decode '(1 2 3 4 5))))
  (is (= '(9 9 9 9 9 9 9 9 9) (decode '([9 9])))))
(run-tests)


