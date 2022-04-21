(ns automata
  (:require [clojure.test :refer [deftest is run-tests]]))

(defrecord DFA [initial-state accept-states transitions])

(def dfa-1 (->DFA :q0
                  #{:q2}
                  {:q0 {\a :q1
                        \b :q0}
                   :q1 {\a :q1
                        \b :q2}
                   :q2 {\a :q2
                        \b :q2}}))
(.initial-state dfa-1)
(.accept-states dfa-1)
(.transitions dfa-1)

(defn accepts?
  [dfa input]
  (let [initial-state (.initial-state dfa)
        accept-states (.accept-states dfa)
        transitions (.transitions dfa)]
    (loop [input (seq input)
           current initial-state]
      (if (empty? input)
        (contains? accept-states current)
        (recur (rest input)
               (; Next state from current using next element of input
                 (transitions current) ; Edges of current state
                 (first input)))))))

(accepts? dfa-1 "abb")

(deftest test-problem1
  (is (accepts? dfa-1 "ab"))
  (is (accepts? dfa-1 "abba"))
  (is (accepts? dfa-1 "aaab"))
  (is (accepts? dfa-1 "abbbbbbbbb"))
  (is (not (accepts? dfa-1 "")))
  (is (not (accepts? dfa-1 "a")))
  (is (not (accepts? dfa-1 "baa")))
  (is (not (accepts? dfa-1 "bbba"))))

; -------------------------------------------
; Problem 2
(def dfa-2 (->DFA :q0
                  #{:q2}
                  {:q0 {\0 :q1
                        \1 :q3}
                   :q1 {\0 :q1
                        \1 :q2}
                   :q2 {\0 :q1
                        \1 :q2}
                   :q3 {\0 :q3
                        \1 :q3}}))

(deftest test-problem2
  (is (accepts? dfa-2 "01"))
  (is (accepts? dfa-2 "0101"))
  (is (accepts? dfa-2 "01111"))
  (is (accepts? dfa-2 "000001"))
  (is (not (accepts? dfa-2 "")))
  (is (not (accepts? dfa-2 "00")))
  (is (not (accepts? dfa-2 "1001011")))
  (is (not (accepts? dfa-2 "1001010"))))

(run-tests)