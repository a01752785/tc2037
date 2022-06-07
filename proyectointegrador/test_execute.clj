;; ------------------------------------------------------------------
;; Pruebas unitarias para la función execute que simula una máquina
;; de Von Neumann.
;; ------------------------------------------------------------------

(ns test-execute
  (:require
    [clojure.test :refer [deftest is run-tests]]
    [proyecto-integrador :refer [execute]]))


(deftest test-hlt
  (is (= "\nProgram terminated.\n"
         (with-out-str
           (execute [0] 128)))))


(deftest test-nop
  (is (= "\nProgram terminated.\n"
         (with-out-str
           (execute [1 1 1 1 0] 128)))))


(deftest test-ld
  (is (= "2 \nProgram terminated.\n"
         (with-out-str
           (execute [2 4 25 0 2] 128)))))


(deftest test-ldi
  (is (= "3 \nProgram terminated.\n"
         (with-out-str
           (execute [4 5 3 25 0 3] 128)))))


(deftest test-ct
  (is (= "5 \nProgram terminated.\n"
         (with-out-str
           (execute [4 5 25 0] 128)))))


(deftest test-st
  (is (= "7 \nProgram terminated.\n"
         (with-out-str
           (execute [4 7 2 8 5 8 25 0 0] 128)))))


(deftest test-sti
  (is (= "11 \nProgram terminated.\n"
         (with-out-str
           (execute [4 11 4 9 6 2 9 25 0 0] 128)))))


(deftest test-pop
  (is (= "13 \nProgram terminated.\n"
         (with-out-str
           (execute [4 13 4 11 7 25 0] 128)))))


(deftest test-swp
  (is (= "17 19 \nProgram terminated.\n"
         (with-out-str
           (execute [4 17 4 19 8 25 25 0] 128)))))


(deftest test-dup
  (is (= "23 23 \nProgram terminated.\n"
         (with-out-str
           (execute [4 23 9 25 25 0] 128)))))


(deftest test-add
  (is (= "29 \nProgram terminated.\n"
         (with-out-str
           (execute [4 -50 4 79 10 25 0] 128)))))


(deftest test-sub
  (is (= "31 \nProgram terminated.\n"
         (with-out-str
           (execute [4 -50 4 -81 11 25 0] 128)))))


(deftest test-mul
  (is (= "37 \nProgram terminated.\n"
         (with-out-str
           (execute [4 -37 4 -1 12 25 0] 128)))))


(deftest test-div
  (is (= "41 \nProgram terminated.\n"
         (with-out-str
           (execute [4 1732 4 42 13 25 0] 128)))))


(deftest test-rem
  (is (= "43 \nProgram terminated.\n"
         (with-out-str
           (execute [4 1135 4 78 14 25 0] 128)))))


(deftest test-eqz
  (is (= "0 1 \nProgram terminated.\n"
         (with-out-str
           (execute [4 0 15 4 42 15 25 25 0] 128)))))


(deftest test-ceq
  (is (= "0 1 \nProgram terminated.\n"
         (with-out-str
           (execute [4 13 4 13 16 4 13 4 12 16 25 25 0] 128)))))


(deftest test-cne
  (is (= "0 1 \nProgram terminated.\n"
         (with-out-str
           (execute [4 13 4 12 17 4 13 4 13 17 25 25 0] 128)))))


(deftest test-clt
  (is (= "0 0 1 \nProgram terminated.\n"
         (with-out-str
           (execute [4 13 4 12 18 25 4 13 4 13
                     18 25 4 12 4 13 18 25 0]
                    128)))))


(deftest test-cle
  (is (= "0 1 1 \nProgram terminated.\n"
         (with-out-str
           (execute [4 13 4 12 19 25 4 13 4 13
                     19 25 4 12 4 13 19 25 0]
                    128)))))


(deftest test-cgt
  (is (= "0 0 1 \nProgram terminated.\n"
         (with-out-str
           (execute [4 12 4 13 20 25 4 13 4 13
                     20 25 4 13 4 12 20 25 0]
                    128)))))


(deftest test-cge
  (is (= "0 1 1 \nProgram terminated.\n"
         (with-out-str
           (execute [4 12 4 13 21 25 4 13 4 13
                     21 25 4 13 4 12 21 25 0]
                    128)))))


(deftest test-jp
  (is (= "47 53 59 \nProgram terminated.\n"
         (with-out-str
           (execute [4 47 25 22 9 4 59 25 0
                     4 53 25 22 5 4 42 25 0] 128)))))


(deftest test-jpc
  (is (= "61 67 71 \nProgram terminated.\n"
         (with-out-str
           (execute [4 0 23 7 4 61 25 4 67 25 4
                     -1 23 17 4 70 25 4 71 25 0] 128)))))


(deftest test-jpi
  (is (= "73 79 \nProgram terminated.\n"
         (with-out-str
           (execute [4 6 24 4 70 25 4 73
                     25 4 12 24 4 79 25 0] 128)))))


(deftest test-out
  (is (= "83 \nProgram terminated.\n"
         (with-out-str
           (execute [4 83 25 0] 128)))))


(deftest test-chr
  (is (= "ABC\nProgram terminated.\n"
         (with-out-str
           (execute [4 67 4 66 4 65 26 26 26 0] 128)))))


(deftest test-hello-world
  (is (= "Hello, World!\nProgram terminated.\n"
         (with-out-str
           (execute [2 19 3 9 15 23 17 26 2 19
                     4 1 10 5 19 22 0 7 0 20 72
                     101 108 108 111 44 32 87
                     111 114 108 100 33 0] 128)))))


(deftest test-5-factorial
  (is (= "120 \nProgram terminated.\n"
         (with-out-str
           (execute [2 29 2 27 20 23 23 2 28 2 29
                     12 5 28 2 29 4 1 10 5 29 22
                     0 2 28 25 0 5 1 1] 128)))))


(deftest test-10-factorial
  (is (= "3628800 \nProgram terminated.\n"
         (with-out-str
           (execute [2 29 2 27 20 23 23 2 28 2 29
                     12 5 28 2 29 4 1 10 5 29 22
                     0 2 28 25 0 10 1 1] 128)))))


(run-tests)
