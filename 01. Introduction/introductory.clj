; Un comentario
(ns introductory
  (:require [clojure.test :refer [deftest is run-tests]])
  (:require [clojure.math.numeric-tower :refer [sqrt]]))

(defn gibibytes->bytes
  "Convert gibibytes input into corresponding bytes."
  [gibibytes]
  (* gibibytes 1024 1024 1024))

(deftest test-gibibytes->bytes
  (is (= 0 (gibibytes->bytes 0)))
  (is (= 1073741824 (gibibytes->bytes 1)))
  (is (= 5368709120 (gibibytes->bytes 5)))
  (is (= 26415122612224 (gibibytes->bytes 24601))))

(defn f2c
  [F]
  (/ (* 5 (- F 32)) 9))

(deftest test-f2c
  (is (= 100.0 (f2c 212.0)))
  (is (= 0.0 (f2c 32.0)))
  (is (= -40.0 (f2c -40.0))))

(defn roots
  [a b c]
  (let [d (- b)
        e (sqrt (- (* b b) (* 4 a c)))
        f (* 2 a)]
    [(/ (+ d e) f)
     (/ (- d e) f)
     ]))

(deftest test-roots
  (is (= [-1 -1] (roots 2 4 2)))
  (is (= [0 0] (roots 1 0 0)))
  (is (= [-1/4 -1] (roots 4 5 1))))

(defn sign
  [number]
  (if (< number 0)
    -1
    (if (= number 0)
       0
       1)))

(deftest test-sign
  (is (= -1 (sign -5)))
  (is (= 1 (sign 10)))
  (is (= 0 (sign 0))))

(run-tests)