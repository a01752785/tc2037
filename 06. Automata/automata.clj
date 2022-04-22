(ns automata
  (:require [clojure.test :refer [deftest is run-tests]]))
(defrecord DFA [initial-state
                accept-states
                transitions])
;Accepts function
(defn accepts?
  [dfa input]
  (let [initial-state (.initial_state dfa)
        accept-states (.accept_states dfa)
        transitions (.transitions dfa)]
    (loop [input (seq input)
           current initial-state]
      (if (empty? input)
        (contains? accept-states current)
        (recur (rest input)
               ((transitions current) (first input)))))))
;Problem 1
(def dfa-1 (->DFA :q0
                  #{:q2}
                  {:q0 {\a :q1
                        \b :q0}
                   :q1 {\a :q1
                        \b :q2}
                   :q2 {\a :q2
                        \b :q2}}))
(deftest test-problem1
  (is (accepts? dfa-1 "ab"))
  (is (accepts? dfa-1 "abba"))
  (is (accepts? dfa-1 "aaab"))
  (is (accepts? dfa-1 "abbbbbbbbb"))
  (is (not (accepts? dfa-1 "")))
  (is (not (accepts? dfa-1 "a")))
  (is (not (accepts? dfa-1 "baa")))
  (is (not (accepts? dfa-1 "bbba"))))
;Problem 2
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
;Problem 3
(def dfa-3 (->DFA :q0
                  #{:q3}
                  {:q0 {\x :q0
                        \y :q1}
                   :q1 {\x :q0
                        \y :q2}
                   :q2 {\x :q0
                        \y :q3}
                   :q3 {\x :q3
                        \y :q3}}))
(deftest test-problem3
  (is (accepts? dfa-3 "yyy"))
  (is (accepts? dfa-3 "xyxyyyx"))
  (is (accepts? dfa-3 "xxxxxyyyyy"))
  (is (accepts? dfa-3 "yyyxxxxyyy"))
  (is (not (accepts? dfa-3 "")))
  (is (not (accepts? dfa-3 "xxx")))
  (is (not (accepts? dfa-3 "yxxyxxy")))
  (is (not (accepts? dfa-3 "xyxyyxyyx"))))
;Problem 4
(def dfa-4 (->DFA :q0
                  #{:q0}
                  {:q0 {\i :q1
                        \j :q1
                        \k :q1}
                   :q1 {\i :q0
                        \j :q0
                        \k :q0}}))
(deftest test-problem4
  (is (accepts? dfa-4 ""))
  (is (accepts? dfa-4 "ji"))
  (is (accepts? dfa-4 "iiiijjjjkkkk"))
  (is (accepts? dfa-4 "kjikjikjikjikjikjikjikji"))
  (is (not (accepts? dfa-4 "i")))
  (is (not (accepts? dfa-4 "ijk")))
  (is (not (accepts? dfa-4 "jjjjjiiiiikkkkk")))
  (is (not (accepts? dfa-4
                     "kjikjikjikjikjikjikjikjikji"))))
;problem 5
(def dfa-5 (->DFA :q0
                  #{:q0 :q1 :q3}
                  {:q0 {\s :q1
                        \t :q3}
                   :q1 {\s :q2
                        \t :q3}
                   :q2 {\s :q2
                        \t :q2}
                   :q3 {\s :q1
                        \t :q2}}))
(deftest test-problem5
  (is (accepts? dfa-5 ""))
  (is (accepts? dfa-5 "s"))
  (is (accepts? dfa-5 "stststs"))
  (is (accepts? dfa-5 "tststststststs"))
  (is (not (accepts? dfa-5 "ss")))
  (is (not (accepts? dfa-5 "ststststt")))
  (is (not (accepts? dfa-5
                     "tstststsststststsssts")))
  (is (not (accepts? dfa-5
                     "tttttttttttttttttttttttttt"))))

;Problem 6
(def dfa-6 (->DFA :q0
                 #{:q0 :q2 :q4}
                 {:q0 {\# :q1
                       \$ :q0}
                  :q1 {\# :q5
                       \$ :q2}
                  :q2 {\# :q1
                       \$ :q3}
                  :q3 {\# :q5
                       \$ :q4}
                  :q4 {\# :q1
                       \$ :q5}
                  :q5 {\# :q5
                       \$ :q5}}))
(deftest test-problem6
  (is (accepts? dfa-6 ""))
  (is (accepts? dfa-6 "$$$"))
  (is (accepts? dfa-6 "$$$$$$$#$#$$$#$"))
  (is (accepts? dfa-6 "#$$$#$#$$$#$#$$$#$#$"))
  (is (not (accepts? dfa-6 "#")))
  (is (not (accepts? dfa-6 "$$#$#$$#$$$")))
  (is (not (accepts? dfa-6 "$$$$$#$###$$$$#")))
  (is (not (accepts? dfa-6 "#$#$#$#$#$$$#$$$#$$$#"))))
;Problem 7
(def dfa-7 (->DFA :q0
                  #{:q2}
                  {:q0 {\@ :q1
                        \% :q0}
                   :q1 {\@ :q2
                        \% :q1}
                   :q2 {\@ :q3
                        \% :q2}
                   :q3 {\@ :q3
                        \% :q3}}))
(deftest test-problem7
  (is (accepts? dfa-7 "@@"))
  (is (accepts? dfa-7 "%@%@%"))
  (is (accepts? dfa-7 "@%%%%%%%%%@%%"))
  (is (accepts? dfa-7 "%%%%%%@@%%%%%%%%%%"))
  (is (not (accepts? dfa-7 "")))
  (is (not (accepts? dfa-7 "%@%")))
  (is (not (accepts? dfa-7 "@@@@@@@@@@@@")))
  (is (not (accepts? dfa-7 "@%%%%@%%%%%@%%%"))))
(run-tests)