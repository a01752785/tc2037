;----------------------------------------------------------
; Problem Set #2: Repetitions
; Date: March 7, 2022.
; Authors:
;          A01752789 Luis Humberto Romero Pérez
;          A01752785 David Damián Galán
;----------------------------------------------------------
(ns repetitions
    (:require [clojure.test :refer [deftest is run-tests]])
    (:require [clojure.algo.generic.math-functions
               :refer [sqr sqrt approx=]]))
;Problem 1
;Recursion
;(defn enlist
;  [s]
;  (if (empty? s)
;    ()
;    (cons (list (first s))
;          (enlist (rest s)))))
;Loop-recur
;(defn enlist
;  [s]
;  (loop [remainder s
;         result ()]
;    (if (empty? remainder)
;      (reverse result)
;      (recur (rest remainder)
;             (cons (list (first remainder))
;                   result)))))
;Usando APIs
(defn enlist
      "Function that surrounds in a list every upper-level
      element of the sequence."
      [s]
      (map list s))
(deftest test-enlist
         (is (= () (enlist ())))
         (is (= '((a) (b) (c)) (enlist '(a b c))))
         (is (= '((1) (2) (3) (4)) (enlist [1 2 3 4])))
         (is (= '(((1 2 3)) (4) ((5)) (7) (8))
                (enlist '((1 2 3) 4 (5) 7 8)))))

;Problem 2
;Recursion
;(defn positives
;  [s]
;  (if (empty? s)
;    ()
;    (if (pos? (first s))
;      (cons (fisrt s)
;            (positives (rest s)))
;      (positives (rest s)))))
;Usando APIs
(defn positives
      "Function that takes a sequence of numbers and returns
      a list with only the positive ones."
      [nums]
      (filter pos? nums))
(deftest test-positives
         (is (= () (positives ())))
         (is (= () (positives [-4 -1 -10 -13 -5])))
         (is (= [3 6] (positives [-4 3 -1 -10 -13 6 -5])))
         (is (= [4 3 1 10 13 6 5] (positives [4 3 1 10 13 6 5]))))

;Problem 3
;Loop-recur
;(defn add-squares
;  [s]
;  (loop [s s
;         r 0]
;    (if (empty? s)
;      r
;      (recur (rest s)
;             (+ r (sqr (first s)))))))
;Recursion
;(defn add-squares
;  [s]
;  (if (empty? s)
;    0
;    (+ (sqr (first s))
;       (add-squares (rest s)))))
;Usando APIs
(defn add-squares
      "Function that returns the sum of the squares of
      all numbers contained in its input list."
      [s]
      (reduce + (map sqr s)))
(deftest test-add-squares
         (is (= 0 (add-squares [])))
         (is (= 25 (add-squares [5])))
         (is (= 30 (add-squares [2 4 1 3])))
         (is (= 385 (add-squares [1 2 3 4 5 6 7 8 9 10]))))

;Problem 4
;Loop-recur
;(defn duplicate
;  [s]
;  (loop [i 0
;         result ()]
;    (if (>= i (count s))
;      result
;      (recur (inc i)
;             (concat result (repeat 2 (nth s i)))))))
(defn dup
      "Auxiliary function for duplicate, returns a list
      with the input element duplicated."
      [x]
      (list x x))
(defn duplicate
      "Function that takes a sequence and returns a new list
      with each element duplicated."
      [s]
      (mapcat dup s))
(deftest test-duplicate
         (is (= [1 1 2 2 3 3 4 4 5 5]
                (duplicate [1 2 3 4 5])))
         (is (= ()
                (duplicate ())))
         (is (= '(a a)
                (duplicate '(a))))
         (is (= '(a a b b c c d d e e f f g g h h)
                (duplicate '(a b c d e f g h)))))

;Problem 5
;Recursion
;(defn fib
;  [n]
;  (if (<= n 1)
;    n
;    (+' (fib (dec n)) (fib (- n 2)))))
;loop-recur
;(defn fib
;  [n]
;  (loop [a 0
;         b 1
;         n n]
;    (if (zero? n)
;      a
;      (recur b
;             (+' a b)
;             (dec n)))))
;Usando APIs
(defn fib-aux
      "Auxiliary function for fib."
      [[a b]]
      [b (+ a b)])
(defn fib
      "Function that takes a positive integer and returns
      the corresponding element of the Fibonacci sequence."
      [n]
      (first (nth (iterate fib-aux [0 1]) n)))
(deftest test-fib
         (is (= 0
                (fib 0)))
         (is (= 1
                (fib 1)))
         (is (= 1
                (fib 2)))
         (is (= 5
                (fib 5)))
         (is (= [0 1 1 2 3 5 8 13 21 34 55 89 144 233 377 610
                 987 1597 2584 4181 6765]
                (map fib (range 21))))
         (is (= 267914296
                (fib 42))))

;Problem 6
(defn pow
      "Function that takes two numbers, returns the result
      of computing a^b."
      [a b]
      (reduce *' (repeat b a)))
(deftest test-pow
         (is (= 1 (pow 0 0)))
         (is (= 0 (pow 0 1)))
         (is (= 1 (pow 5 0)))
         (is (= 5 (pow 5 1)))
         (is (= 125 (pow 5 3)))
         (is (= 25 (pow -5 2)))
         (is (= -125 (pow -5 3)))
         (is (= 1024 (pow 2 10)))
         (is (= 525.21875 (pow 3.5 5)))
         (is (= 129746337890625 (pow 15 12)))
         (is (= 3909821048582988049 (pow 7 22)))
         (is (= 1267650600228229401496703205376N (pow 2 100))))

;Problem 7
(defn only-symbols?
      "Function that takes a sequence, returns true if all the elements
      (possibly zero) contained are symbols, false otherwise."
      [s]
      (every? symbol? s))
(deftest test-only-symbols?
         (is (= true (only-symbols? [])))
         (is (= true (only-symbols? '(a))))
         (is (= true (only-symbols? '(a b c d e))))
         (is (= false (only-symbols? '(a b c d 42 e))))
         (is (= false (only-symbols? '(42 a b c))))
         (is (= false (only-symbols? [4 8 15 16 23 42]))))

;Problem 8
(defn invert-pairs
      "Function that takes a sequence of vectors containing two
      elements each. Returns a new list with every vector pair inverted."
      [s]
      (map reverse s))
(deftest test-invert-pairs
         (is (= () (invert-pairs ())))
         (is (= '([y x]) (invert-pairs '([x y]))))
         (is (= '([1 a] [2 a] [1 b] [2 b])
                (invert-pairs '([a 1] [a 2] [b 1] [b 2]))))
         (is (= '([1 January] [2 February] [3 March])
                (invert-pairs '([January 1] [February 2] [March 3])))))

;Problem 9
(defn replic
      "Function that takes an integer number(n) and a sequence(s), returns
      a new list that replicates n times each element in s."
      [n s]
      (loop [i 0
             result ()]
            (if (>= i (count s))
              result
              (recur (inc i)
                     (concat result (repeat n (nth s i)))))))
(deftest test-replic
         (is (= () (replic 7 [])))
         (is (= () (replic 0 '(a b c))))
         (is (= '(a a a) (replic 3 '(a))))
         (is (= [1 1 1 1 2 2 2 2 3 3 3 3 4 4 4 4]
                (replic 4 [1 2 3 4]))))

;Problem 10
(defn dot-product
      "Function that takes two sequences a and b, returns the
      result of performing the dot product of a times b."
      [a b]
      (loop [sum 0
             i 0]
            (if (>= i (count a))
              sum
              (recur (+ sum (* (nth a i) (nth b i)))
                     (inc i)))))
(deftest test-dot-product
         (is (= 0 (dot-product [] [])))
         (is (= 42 (dot-product [6] [7])))
         (is (= 32 (dot-product [1 2 3] [4 5 6])))
         (is (= 21.45 (dot-product [1.3 3.4 5.7 9.5 10.4]
                                   [-4.5 3.0 1.5 0.9 0.0]))))

;Problem 11
(defn average
      "Function that takes a sequence of numbers and returns the
      arithmetic mean of the numbers, or nil if it is empty."
      [s]
      (if (empty? s)
        nil
        (loop [sum 0
               i 0]
              (if (>= i (count s))
                (/ sum (count s))
                (recur (+ sum (nth s i))
                       (inc i))))))
(deftest test-average
         (is (nil? (average [])))
         (is (= 4
                (average [4])))
         (is (= 3
                (average [5 6 1 6 0 1 2])))
         (is (= 2.5
                (average [1.7 4.5 0.0 2.0 3.4 5.0 2.5 2.2 1.2]))))

;Problem 12
(defn standard-deviation
      "Function that takes a sequence of numbers and returns the
      population standard deviation of its numbers, or nil if it is empty."
      [s]
      (if (empty? s)
        nil
        (loop [sqrt-sum 0
               i 0
               avg (average s)]
              (if (>= i (count s))
                (sqrt (/ sqrt-sum (count s)))
                (recur (+ sqrt-sum (*
                                     (- (nth s i) avg)
                                     (- (nth s i) avg)))
                       (inc i)
                       avg)))))
(deftest test-standard-deviation
         (is (nil? (standard-deviation [])))
         (is (approx= 1.87
                      (standard-deviation [6 2 3 1])
                      0.01))
         (is (approx= 12.3153
                      (standard-deviation [4 8 15 16 23 42])
                      0.0001))
         (is (approx= 7.07106
                      (standard-deviation [110 105 90 100 95])
                      0.00001))
         (is (approx= 2.983
                      (standard-deviation [9 2 5 4 12 7 8 11
                                           9 3 7 4 12 5 4 10
                                           9 6 9 4])
                      0.001)))

(run-tests)
