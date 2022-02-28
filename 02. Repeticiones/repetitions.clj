(ns repetitions
  (:require [clojure.test :refer [deftest is run-tests]])
  (:require [clojure.algo.generic.math-functions
             :refer [sqr sqrt approx=]]))


;(defn enlist
;  [s]
;  (if (empty? s)
;    ()
;    (cons (list (first s))
;          (enlist (rest s)))))

;(defn enlist
;  [s]
;  (loop [remainder s
;         result ()]
;    (if (empty? remainder)
;      (reverse result)
;      (recur (rest remainder)
;             (cons (list (first remainder))
;                   result)))))

(defn enlist
  [s]
  (map list s))

(enlist '(1 2 3 4))
(deftest test-enlist
  (is (= () (enlist ())))
  (is (= '((a) (b) (c)) (enlist '(a b c))))
  (is (= '((1) (2) (3) (4)) (enlist [1 2 3 4])))
  (is (= '(((1 2 3)) (4) ((5)) (7) (8))
         (enlist '((1 2 3) 4 (5) 7 8)))))

(defn positives
  [s]
  (filter pos? s))

(deftest test-positives
  (is (= () (positives ())))
  (is (= () (positives [-4 -1 -10 -13 -5])))
  (is (= [3 6] (positives [-4 3 -1 -10 -13 6 -5])))
  (is (= [4 3 1 10 13 6 5] (positives [4 3 1 10 13 6 5]))))

(defn add-squares
  [s]
  (reduce + (map sqr s)))

;(defn add-squares
;  [s]
;  (if (empty? s)
;    0
;    (+ (sqr (first s))
;       (add-squares (rest s)))))
;
;(defn add-squares
;  [s]
;  (loop [s s
;         r 0]
;    (if (empty? s)
;      r
;      (recur (rest s)
;             (+ r (sqr (first s)))))))

(deftest test-add-squares
  (is (= 0 (add-squares [])))
  (is (= 25 (add-squares [5])))
  (is (= 30 (add-squares [2 4 1 3])))
  (is (= 385 (add-squares [1 2 3 4 5 6 7 8 9 10]))))

(defn only-symbols?
  [s]
  (every? symbol? s))

(deftest test-only-symbols?
  (is (= true (only-symbols? [])))
  (is (= true (only-symbols? '(a))))
  (is (= true (only-symbols? '(a b c d e))))
  (is (= false (only-symbols? '(a b c d 42 e))))
  (is (= false (only-symbols? '(42 a b c))))
  (is (= false (only-symbols? [4 8 15 16 23 42]))))

(defn invert-pairs
  [s]
  (map reverse s))
(deftest test-invert-pairs
  (is (= () (invert-pairs ())))
  (is (= '([y x]) (invert-pairs '([x y]))))
  (is (= '([1 a][2 a][1 b][2 b])
         (invert-pairs '([a 1][a 2][b 1][b 2]))))
  (is (= '([1 January][2 February][3 March])
         (invert-pairs '([January 1][February 2][March 3])))))

(defn replic
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

(defn dot-product
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

(defn average
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

(defn standard-deviation
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