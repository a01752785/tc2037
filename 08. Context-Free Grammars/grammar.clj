;----------------------------------------------------------
; Problem Set #8: Context-Free Grammars
; Date: June 6, 2022.
; Authors:
;          A01752789 Luis Humberto Romero Perez
;          A01752785 David Damian Galan
;----------------------------------------------------------
(ns grammar
  (:require [clojure.test :refer [deftest is run-tests]])
  (:require [instaparse.core :refer [parser]])
  (:import (instaparse.gll Failure)))

(defn fails? [r] (instance? Failure r))
(defn succeeds? [r] (not (fails? r)))

;Problem 1
(def start-and-end
  "A language with alphabet Σ={#,$} that accepts all nonempty
  strings that start and end with the same symbol."
  (parser "
  S = '$'
      | '#'
      | '$' T '$'
      | '#' T '#'
  T = epsilon
      | T '$'
      | T '#'
"))
(deftest test-start-and-end
  (is (succeeds? (start-and-end "$")))
  (is (succeeds? (start-and-end "#")))
  (is (succeeds? (start-and-end "$$")))
  (is (succeeds? (start-and-end "##")))
  (is (succeeds? (start-and-end "$$$$$$$#$$#$$#$##$")))
  (is (succeeds? (start-and-end "#$$$#$$$$#$$$$#$####")))
  (is (fails? (start-and-end "")))
  (is (fails? (start-and-end "$#")))
  (is (fails? (start-and-end "#$")))
  (is (fails? (start-and-end "###$$#$#$$$#$$$####$")))
  (is (fails? (start-and-end "$#$#$#$$$$#$$$#$$$$#$$#")))
  (is (fails? (start-and-end "#######################$"))))

;Problem 2
(def palindrome
  "A language that accepts a possibly empty sequence of zeros
  and ones that happens to be a palindrome."
  (parser "
  S = epsilon
    | '0'
    | '1'
    | '0' S '0'
    | '1' S '1'
"))
(deftest test-palindrome
  (is (succeeds? (palindrome "")))
  (is (succeeds? (palindrome "0")))
  (is (succeeds? (palindrome "1")))
  (is (succeeds? (palindrome "11")))
  (is (succeeds? (palindrome "00")))
  (is (succeeds? (palindrome "010")))
  (is (succeeds? (palindrome "1111111")))
  (is (succeeds? (palindrome "000010000")))
  (is (succeeds? (palindrome "01001110101110010")))
  (is (fails? (palindrome "01")))
  (is (fails? (palindrome "10")))
  (is (fails? (palindrome "1010")))
  (is (fails? (palindrome "10000000")))
  (is (fails? (palindrome "00010001")))
  (is (fails? (palindrome "1010011010")))
  (is (fails? (palindrome "111111111111111111110"))))

;Problem 3
(def balanced-parentheses
  "A language with alphabet Σ={(,)} that accepts all strings
  containing balanced parentheses. A string of parentheses
  is balanced if each left parenthesis has a matching right
  parenthesis and the matched pairs are well nested."
  (parser "
  S = epsilon
    |  S '(' S ')'
"))
(deftest test-balanced-parentheses
  (is (succeeds? (balanced-parentheses "")))
  (is (succeeds? (balanced-parentheses "()")))
  (is (succeeds? (balanced-parentheses "((((()))))")))
  (is (succeeds? (balanced-parentheses "()()()()()")))
  (is (succeeds? (balanced-parentheses
                   "(()()())((())(()()()))(((())))")))
  (is (fails? (balanced-parentheses "(")))
  (is (fails? (balanced-parentheses ")")))
  (is (fails? (balanced-parentheses "((((())))")))
  (is (fails? (balanced-parentheses "))))((((")))
  (is (fails? (balanced-parentheses
                "(()()())((())(()()()))((((())))"))))

;Problem 4
(def o-in-the-middle
  "A language with alphabet Σ={o,x} that accepts all strings
  with an odd length and o as its symbol in the middle."
  (parser "
  S = 'o'
    | T S T
  T = 'o'
    | 'x'
"))
(deftest test-o-in-the-middle
  (is (succeeds? (o-in-the-middle "o")))
  (is (succeeds? (o-in-the-middle "xox")))
  (is (succeeds? (o-in-the-middle "oooooxxxx")))
  (is (succeeds? (o-in-the-middle "oxxoooxooxoxoxx")))
  (is (succeeds? (o-in-the-middle "ooooooooooooooo")))
  (is (succeeds? (o-in-the-middle "xxxxxxxxxxoxxxxxxxxxx")))
  (is (fails? (o-in-the-middle "")))
  (is (fails? (o-in-the-middle "ox")))
  (is (fails? (o-in-the-middle "oxo")))
  (is (fails? (o-in-the-middle "oxxoooxxoxoxoxx")))
  (is (fails? (o-in-the-middle "xxxxxxxxxxxxxxxxxxxxx")))
  (is (fails? (o-in-the-middle "oooooooooooooooooooooo"))))

;Problem 5
(def twice
  "A language that accepts a nonempty sequence of consecutive
  symbols x followed by a nonempty sequence of consecutive
  symbols y. The number of symbols x has to be exactly twice
  the number of symbols y."
  (parser "
  S = X T 'y'
  T = epsilon
    | S
  X = 'x' 'x'
"))
(deftest test-twice
  (is (succeeds? (twice "xxy")))
  (is (succeeds? (twice "xxxxyy")))
  (is (succeeds? (twice "xxxxxxxxyyyy")))
  (is (succeeds? (twice "xxxxxxxxxxxxxxxxxxxxyyyyyyyyyy")))
  (is (fails? (twice "")))
  (is (fails? (twice "xy")))
  (is (fails? (twice "yxx")))
  (is (fails? (twice "xyy")))
  (is (fails? (twice "xxxyyy")))
  (is (fails? (twice "xyyxyy")))
  (is (fails? (twice "xxxxyyx")))
  (is (fails? (twice "yyyxxxxxx")))
  (is (fails? (twice "xxxxxxxxxxyy")))
  (is (fails? (twice "xyxyxyxyxxxx")))
  (is (fails? (twice "xxxxxxxxxxyyyy"))))

;Problem 6
(def a-plus-b-equals-c
  "This language accepts a possibly empty sequence of
  consecutive symbols a followed by a possibly empty sequence
  of consecutive symbols b followed by a possibly empty sequence
  of consecutive symbols c. The number of symbols c has to be
  exactly the number symbols a plus the number symbols b."
  (parser "
 S = epsilon
  | 'a' S 'c'
  | 'b' S 'c'
"))
(deftest test-a-plus-b-equals-c
  (is (succeeds? (a-plus-b-equals-c "")))
  (is (succeeds? (a-plus-b-equals-c "abcc")))
  (is (succeeds? (a-plus-b-equals-c "ac")))
  (is (succeeds? (a-plus-b-equals-c "bc")))
  (is (succeeds? (a-plus-b-equals-c "aaaaabbbcccccccc")))
  (is (succeeds? (a-plus-b-equals-c "aaaaaaaaabcccccccccc")))
  (is (succeeds? (a-plus-b-equals-c
                   "bbbbbbbbbbbbbccccccccccccc")))
  (is (succeeds? (a-plus-b-equals-c
                   "abbbbbbbbbbbbccccccccccccc")))
  (is (succeeds? (a-plus-b-equals-c
                   "aaaabbbbbbbbbbbccccccccccccccc")))
  (is (fails? (a-plus-b-equals-c "a")))
  (is (fails? (a-plus-b-equals-c "b")))
  (is (fails? (a-plus-b-equals-c "c")))
  (is (fails? (a-plus-b-equals-c "ab")))
  (is (fails? (a-plus-b-equals-c "abc")))
  (is (fails? (a-plus-b-equals-c "abbcc")))
  (is (fails? (a-plus-b-equals-c "cccccc")))
  (is (fails? (a-plus-b-equals-c "ccccaabb")))
  (is (fails? (a-plus-b-equals-c "aaaaaaaa")))
  (is (fails? (a-plus-b-equals-c "bbbbbbbbb")))
  (is (fails? (a-plus-b-equals-c "aaaaaabbbbbb")))
  (is (fails? (a-plus-b-equals-c "aabbcccccccccccccccccc"))))
(run-tests)
