;----------------------------------------------------------
; Problem Set #4: Higher-Order Functions
; Date: March 31, 2022.
; Authors:
;          A01752789 Luis Humberto Romero Pérez
;          A01752785 David Damián Galán
;----------------------------------------------------------
(ns higher-order-functions
  (:require [clojure.test :refer [deftest is run-tests]])
  (:require [clojure.algo.generic.math-functions
             :refer [abs approx=]]))
;Problem 1
(defn argswap
  "Function that takes a two argument function f and returns a new
  function that behaves like f but with its two arguments swapped."
  [f]
  (fn [x y] (f y x)))
(deftest test-argswap
  (is (= '(2 1)
         ((argswap list) 1 2)))
  (is (= -7
         ((argswap -) 10 3)))
  (is (= 1/4
         ((argswap /) 8 2)))
  (is (= '((4 5 6) 1 2 3)
         ((argswap cons) '(1 2 3) '(4 5 6))))
  (is (= '(1 0 4 25 100)
         ((argswap map) '(-1 0 2 5 10) #(* % %)))))

;Problem 2
(defn there-exists-one
  "Function that takes two inputs: a one argument predicate
  function pred? and a sequence s. Returns true if there is
  exactly one element in s that satisfies pred?,
  otherwise returns false."
  [pred? s]
  (if (= (count (filter pred? s)) 1)
    true
    false))
(deftest test-there-exists-one
  (is (not (there-exists-one pos?
                             ())))
  (is (there-exists-one pos?
                        '(-1 -10 4 -5 -2 -1)))
  (is (there-exists-one neg?
                        '(-1)))
  (is (not (there-exists-one symbol?
                             '(4 8 15 16 23 42))))
  (is (there-exists-one symbol?
                        '(4 8 15 sixteen 23 42))))
;Problem 3
(defn linear-search
  "Function that sequentially searches for a value x,
  given an equality function eq-fun and a vector vct.
  Returns the index if the element was found and nil otherwise."
  [vct x eq-fun]
  (let [n (count vct)]
    (loop [i 0]
      (cond
        (= i n)
        nil
        (eq-fun x (vct i))
        i
        :else
        (recur (inc i))))))
(deftest test-linear-search
  (is (nil? (linear-search [] 5 =)))
  (is (= 0 (linear-search [5] 5 =)))
  (is (= 4 (linear-search
             [48 77 30 31 5 20 91 92
              69 97 28 32 17 18 96]
             5
             =)))
  (is (= 3 (linear-search
             ["red" "blue" "green" "black" "white"]
             "black"
             identical?)))
  (is (nil? (linear-search
              [48 77 30 31 5 20 91 92
               69 97 28 32 17 18 96]
              96.0
              =)))
  (is (= 14 (linear-search
              [48 77 30 31 5 20 91 92
               69 97 28 32 17 18 96]
              96.0
              ==)))
  (is (= 8 (linear-search
             [48 77 30 31 5 20 91 92
              69 97 28 32 17 18 96]
             70
             #(<= (abs (- %1 %2)) 1)))))

;Problem 4
(defn deriv
  "Returns a function corresponding to the derivative
   of a continuous function f.
  It takes h as step of change."
  [f h]
  (fn [x]
    (/ (- (f (+ x h)) (f x)) h)))
(defn f [x] (* x x x))
(def df (deriv f 0.001))
(def ddf (deriv df 0.001))
(def dddf (deriv ddf 0.001))

(deftest test-deriv
  (is (approx= 75 (df 5) 0.05))
  (is (approx= 30 (ddf 5) 0.05))
  (is (approx= 6 (dddf 5) 0.05)))

;Problem 5
(defn newton
  "Implements the Newton's method to find a root
  of a function f according to the mathematical definition.
  It takes n as the number of steps."
  [f n]
  (if (zero? n)
    0
    (let [x-n-1 (newton f (dec n))
          f' (deriv f 0.001)]
      (- x-n-1 (/ (f x-n-1)
                  (f' x-n-1))))))
(deftest test-newton
  (is (approx= 10.0
               (newton (fn [x] (- x 10))
                       1)
               0.00001))
  (is (approx= -0.5
               (newton (fn [x] (+ (* 4 x) 2))
                       1)
               0.00001))
  (is (approx= -1.0
               (newton (fn [x] (+ (* x x x) 1))
                       50)
               0.00001))
  (is (approx= -1.02987
               (newton (fn [x] (+ (Math/cos x)
                                  (* 0.5 x)))
                       5)
               0.00001)))

;Problem 6
(defn integral
  "Implements the Simpson's rule for numeric integration
  according to the mathematical definition. It takes
  a and b as parameters for the lower and upper bound of
  the integral respectively and n, the number of steps."
  [a b n f]
  (let [h (/ (- b a) n)]
    (loop [k 0
           sum 0]
      (cond
        (> k n) (/ (* h sum) 3)
        (zero? k) (recur (inc k) (+ sum (f a)))
        (= k n) (recur (inc k) (+ sum (f (+ a (* k h)))))
        (= (mod k 2) 0) (recur (inc k) (+ sum (* 2 (f (+ a (* k h))))))
        :else (recur (inc k) (+ sum (* 4 (f (+ a (* k h))))))))))
(deftest test-integral
  (is (= 1/4 (integral 0 1 10 (fn [x] (* x x x)))))
  (is (= 21/4
         (integral 1 2 10
                   (fn [x]
                     (integral 3 4 10
                               (fn [y]
                                 (* x y))))))))
;Problem 7
(defn binary-search
  "Implements the binary search algorithm given a sorted vector
  in ascending order with no repetitions, a target value x
  and a less than function lt-fun. Returns the index where the
  element is located if it was found and nil otherwise."
  [vct x lt-fun]
  (loop [beg 0
         end (dec (count vct))]
    (let [mid (quot (+' beg end) 2)]
      (cond (> beg end) nil
            (lt-fun (vct mid) x) (recur (inc mid) end)
            (lt-fun x (vct mid)) (recur beg (dec mid))
            :else mid))))

(def small-list [4 8 15 16 23 42])

(def big-list [0 2 5 10 11 13 16 20 24 26
               29 30 31 32 34 37 40 43 44
               46 50 53 58 59 62 63 66 67
               70 72 77 79 80 83 85 86 94
               95 96 99])

(def animals ["dog" "dragon" "horse" "monkey" "ox"
              "pig" "rabbit" "rat" "rooster" "sheep"
              "snake" "tiger"])
(defn str<
  "Returns true if a is less than b, otherwise
   returns false. Designed to work with strings."
  [a b]
  (< (compare a b) 0))
(deftest test-binary-search
  (is (nil? (binary-search [] 5 <)))
  (is (= 3 (binary-search small-list 16 <)))
  (is (= 0 (binary-search small-list 4 <)))
  (is (= 5 (binary-search small-list 42 <)))
  (is (nil? (binary-search small-list 7 <)))
  (is (nil? (binary-search small-list 2 <)))
  (is (nil? (binary-search small-list 99 <)))
  (is (= 17 (binary-search big-list 43 <)))
  (is (= 0 (binary-search big-list 0 <)))
  (is (= 39 (binary-search big-list 99 <)))
  (is (nil? (binary-search big-list 12 <)))
  (is (nil? (binary-search big-list -1 <)))
  (is (nil? (binary-search big-list 100 <)))
  (is (= 5 (binary-search animals "pig" str<)))
  (is (= 0 (binary-search animals "dog" str<)))
  (is (= 11 (binary-search animals "tiger" str<)))
  (is (nil? (binary-search animals "elephant" str<)))
  (is (nil? (binary-search animals "alligator" str<)))
  (is (nil? (binary-search animals "unicorn" str<))))
(run-tests)
