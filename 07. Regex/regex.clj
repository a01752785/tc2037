(ns regex
  (:require [clojure.test :refer [deftest is run-tests]]))
;;;Regular expression 1:
(def c-identifier #"[a-zA-Z_]\w*")
(deftest test-c-identifiers
  (is (re-matches c-identifier "_"))
  (is (re-matches c-identifier "a"))
  (is (re-matches c-identifier "A"))
  (is (re-matches c-identifier "_an_identfier_42"))
  (is (re-matches c-identifier "_1234567890"))
  (is (re-matches c-identifier "___________"))
  (is (re-matches c-identifier "ThisIsAnIdentfier"))
  (is (not (re-matches c-identifier "")))
  (is (not (re-matches c-identifier "5")))
  (is (not (re-matches c-identifier "1234567890")))
  (is (not (re-matches c-identifier "#!@$^")))
  (is (not (re-matches c-identifier "_a_b_c_$"))))
;;; Regular expression 2:
;(def scheme-boolean #"#([tf]|(true|false))")
(def scheme-boolean #"#(t(rue)?|f(alse)?)")
(deftest test-scheme-boolean
  (is (re-matches scheme-boolean "#t"))
  (is (re-matches scheme-boolean "#f"))
  (is (re-matches scheme-boolean "#true"))
  (is (re-matches scheme-boolean "#false"))
  (is (not (re-matches scheme-boolean "t")))
  (is (not (re-matches scheme-boolean "f")))
  (is (not (re-matches scheme-boolean "true")))
  (is (not (re-matches scheme-boolean "false")))
  (is (not (re-matches scheme-boolean "()")))
  (is (not (re-matches scheme-boolean "0")))
  (is (not (re-matches scheme-boolean "T")))
  (is (not (re-matches scheme-boolean "F")))
  (is (not (re-matches scheme-boolean "#v")))
  (is (not (re-matches scheme-boolean "#truth")))
  (is (not (re-matches scheme-boolean "#falsy"))))
;;: Regular expression 3:
(def scheme-integer #"\d+|#(b[01]+|o[0-7]+|d\d+|x[0-9a-fA-F]+)")
(deftest test-scheme-integer
  (is (re-matches scheme-integer "0"))
  (is (re-matches scheme-integer "24601"))
  (is (re-matches scheme-integer "#d1234567890"))
  (is (re-matches scheme-integer "#b10"))
  (is (re-matches scheme-integer "#o12345670"))
  (is (re-matches scheme-integer "#x1234567890abcdefABCDEF"))
  (is (not (re-matches scheme-integer "")))
  (is (not (re-matches scheme-integer "#123")))
  (is (not (re-matches scheme-integer "#da1234567890")))
  (is (not (re-matches scheme-integer "#b102")))
  (is (not (re-matches scheme-integer "#o123456780")))
  (is (not (re-matches scheme-integer
                       "#x1234567890abcdefgABCDEF"))))
;;; Regular expression 4:
;(def java-integer #"(0[0-7]+|(0|[1-9]\d*)|0[xX][0-9a-fA-F]+)([lL]?)")
(def java-integer #"(0([0-7]+|[xX][0-9a-fA-F]+)?|[1-9]\d*)[lL]?")
(deftest test-java-integer
  (is (re-matches java-integer "0"))
  (is (re-matches java-integer "1234567890"))
  (is (re-matches java-integer "012345670"))
  (is (re-matches java-integer "0xabcdef1234567890ABCDEF"))
  (is (re-matches java-integer "0l"))
  (is (re-matches java-integer "1234567890l"))
  (is (re-matches java-integer "012345670L"))
  (is (re-matches java-integer "0Xabcde1234567890fABCDEFL"))
  (is (not (re-matches java-integer "")))
  (is (not (re-matches java-integer "L")))
  (is (not (re-matches java-integer "1a234567890")))
  (is (not (re-matches java-integer "0123456780")))
  (is (not (re-matches java-integer "0x1234567890abcdefgABCD"))))
;;: Regular expression 5:
(def java-float #"")
;;: Regular expression 6:
;(def c-comment #"[/][*][^*]*[*]+([^*/][^*]*[*]+)*[/]")
(def c-comment #"([/][*])(.|\n)*?([*][/])")
(deftest test-c-comment
  (is (re-matches c-comment "/**/"))
  (is (re-matches c-comment "/*-*/"))
  (is (re-matches c-comment "/*\n*/"))
  (is (re-matches c-comment
                  "/***********
                   /*         *
                   /*         *
                   /***********/"))
  (is (= 3 (count (re-seq c-comment "/*********
                                      Comment 1
                                      *********/

                                     /*********
                                      Comment 2
                                      *********/

                                     /*********
                                      Comment 3
                                      *********/"))))
  (is (not (re-matches c-comment "/")))
  (is (not (re-matches c-comment "/*")))
  (is (not (re-matches c-comment "/**")))
  (is (not (re-matches c-comment "/*/")))
  (is (not (re-matches c-comment "//")))
  (is (not (re-matches c-comment "/** /")))
  (is (not (re-matches c-comment "******/")))
  (is (not (re-matches c-comment "/ * * * */"))))
(run-tests)