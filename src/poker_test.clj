(ns poker-test
  (:require [clojure.test :refer :all]
            [poker :refer :all]))

(def high-seven ["2H" "3S" "4C" "5C" "7D"])
(def pair-hand ["2H" "2S" "4C" "5C" "7D"])
(def two-pairs-hand ["2H" "2S" "4C" "4D" "7D"])
(def three-of-a-kind-hand ["2H" "2S" "2C" "4D" "7D"])
(def four-of-a-kind-hand ["2H" "2S" "2C" "2D" "7D"])
(def straight-hand ["2H" "3S" "6C" "5D" "4D"])
(def low-ace-straight-hand ["2H" "3S" "4C" "5D" "AD"])
(def high-ace-straight-hand ["TH" "AS" "QC" "KD" "JD"])
(def flush-hand ["2H" "4H" "5H" "9H" "7H"])
(def full-house-hand ["2H" "5D" "2D" "2C" "5S"])
(def straight-flush-hand ["2H" "3H" "6H" "5H" "4H"])
(def low-ace-straight-flush-hand ["2D" "3D" "4D" "5D" "AD"])
(def high-ace-straight-flush-hand ["TS" "AS" "QS" "KS" "JS"])

(println "Winning hand:" (winning-hand [high-seven pair-hand two-pairs-hand three-of-a-kind-hand four-of-a-kind-hand straight-hand flush-hand full-house-hand straight-flush-hand]))

(deftest card-order-test
  (testing "card-order function"
    (is (= 14 (card-order "AS")))
    (is (= 13 (card-order "KH")))
    (is (= 10 (card-order "TD")))
    (is (= 2 (card-order "2C")))
    (is (= 7 (card-order "7S")))))

(deftest sort-cards-test
  (testing "sort-cards function"
    (is (= ["2H" "5C" "7D" "KS" "AC"] (sort-cards ["AC" "2H" "KS" "7D" "5C"])))
    (is (= ["2H" "3S" "4C" "5D" "6H"] (sort-cards ["3S" "2H" "6H" "4C" "5D"])))
    (is (= ["4H" "4S" "4C" "4D" "AH"] (sort-cards ["4H" "AH" "4S" "4C" "4D"])))
    (is (= ["2H" "3H" "4H" "5H" "6H"] (sort-cards ["4H" "2H" "6H" "3H" "5H"])))
    (is (= ["TH" "JH" "QH" "KH" "AH"] (sort-cards ["KH" "QH" "AH" "JH" "TH"])))))

;; Basic card property tests
(deftest suit-test
  (testing "suit function"
    (is (= "H" (suit "AH")))
    (is (= "S" (suit "2S")))
    (is (= "D" (suit "TD")))
    (is (= "C" (suit "KC")))
    (is (= "H" (suit "5H")))))

(deftest rank-test
  (testing "rank function"
    (is (= 14 (rank "AS")))
    (is (= 10 (rank "TH")))
    (is (= 13 (rank "KD")))
    (is (= 2 (rank "2C")))
    (is (= 7 (rank "7H")))))

;; Hand category tests
(deftest pair?-test
  (testing "pair? function"
    (is (true? (pair? ["2H" "2S" "4C" "5C" "7D"])))
    (is (true? (pair? ["AH" "AS" "4C" "5C" "7D"])))
    (is (false? (pair? ["2H" "3S" "4C" "5C" "7D"])))
    (is (true? (pair? ["TH" "TS" "4C" "5C" "7D"])))
    (is (false? (pair? ["2H" "3H" "4H" "5H" "7H"])))))

(deftest three-of-a-kind?-test
  (testing "three-of-a-kind? function"
    (is (true? (three-of-a-kind? ["2H" "2S" "2C" "5C" "7D"])))
    (is (true? (three-of-a-kind? ["AH" "AS" "AC" "5C" "7D"])))
    (is (false? (three-of-a-kind? ["2H" "2S" "4C" "5C" "7D"])))
    (is (true? (three-of-a-kind? ["TH" "TS" "TC" "5C" "7D"])))
    (is (false? (three-of-a-kind? ["2H" "3H" "4H" "5H" "7H"])))))

(deftest four-of-a-kind?-test
  (testing "four-of-a-kind? function"
    (is (true? (four-of-a-kind? ["2H" "2S" "2C" "2D" "7D"])))
    (is (true? (four-of-a-kind? ["AH" "AS" "AC" "AD" "7D"])))
    (is (false? (four-of-a-kind? ["2H" "2S" "2C" "5C" "7D"])))
    (is (true? (four-of-a-kind? ["TH" "TS" "TC" "TD" "7D"])))
    (is (false? (four-of-a-kind? ["2H" "3H" "4H" "5H" "7H"])))))

(deftest flush?-test
  (testing "flush? function"
    (is (true? (flush? ["2H" "4H" "6H" "8H" "TH"])))
    (is (true? (flush? ["AS" "KS" "QS" "JS" "TS"])))
    (is (false? (flush? ["2H" "4H" "6H" "8H" "TC"])))
    (is (true? (flush? ["2D" "4D" "6D" "8D" "TD"])))
    (is (false? (flush? ["2H" "2S" "2C" "2D" "7D"])))))

(deftest full-house?-test
  (testing "full-house? function"
    (is (true? (full-house? ["2H" "2S" "2C" "3D" "3H"])))
    (is (true? (full-house? ["AH" "AS" "AC" "KD" "KH"])))
    (is (false? (full-house? ["2H" "2S" "2C" "3D" "4H"])))
    (is (true? (full-house? ["TH" "TS" "TC" "7D" "7H"])))
    (is (false? (full-house? ["2H" "3H" "4H" "5H" "7H"])))))

(deftest two-pairs?-test
  (testing "two-pairs? function"
    (is (true? (two-pairs? ["2H" "2S" "3C" "3D" "7D"])))
    (is (true? (two-pairs? ["AH" "AS" "KC" "KD" "2D"])))
    (is (false? (two-pairs? ["2H" "2S" "3C" "4D" "7D"])))
    (is (true? (two-pairs? ["TH" "TS" "9C" "9D" "7D"])))
    (is (false? (two-pairs? ["2H" "3H" "4H" "5H" "7H"])))))

(deftest straight?-test
  (testing "straight? function"
    (is (true? (straight? ["2H" "3S" "4C" "5D" "6H"])))
    (is (true? (straight? ["TH" "JS" "QC" "KD" "AH"])))
    (is (true? (straight? ["AH" "2S" "3C" "4D" "5H"])))
    (is (false? (straight? ["2H" "3S" "4C" "5D" "7H"])))
    (is (false? (straight? ["2H" "2S" "2C" "2D" "7D"])))))

(deftest straight-flush?-test
  (testing "straight-flush? function"
    (is (true? (straight-flush? ["2H" "3H" "4H" "5H" "6H"])))
    (is (true? (straight-flush? ["TS" "JS" "QS" "KS" "AS"])))
    (is (true? (straight-flush? ["AH" "2H" "3H" "4H" "5H"])))
    (is (false? (straight-flush? ["2H" "3H" "4H" "5H" "7H"])))
    (is (false? (straight-flush? ["2H" "3S" "4C" "5D" "6H"])))))

(deftest value-test
  (testing "value function"
    (is (= '8 (value straight-flush-hand)))
    (is (= '7 (value four-of-a-kind-hand)))
    (is (= '6 (value full-house-hand)))
    (is (= '5 (value flush-hand)))
    (is (= '4 (value straight-hand)))))

(deftest kickers-test
  (testing "kickers function"
    (is (= [14 13 12 11 10] (kickers ["AS" "KS" "QS" "JS" "TS"])))
    (is (= [2 7] (kickers ["2H" "2S" "2C" "2D" "7D"])))
    (is (= [2 5] (kickers full-house-hand)))
    (is (= [2 7 5 4] (kickers pair-hand)))
    (is (= [14 13 12 11 10] (kickers high-ace-straight-hand)))))

(deftest higher-kicker?-test
  (testing "higher-kicker? function"
    (is (true? (higher-kicker? [14 13 12 11 10] [13 12 11 10 9])))
    (is (false? (higher-kicker? [13 12 11 10 9] [14 13 12 11 10])))
    (is (false? (higher-kicker? [14 13 12 11 10] [14 13 12 11 10])))
    (is (true? (higher-kicker? [7 6 5 4 3] [7 6 5 4 2])))
    (is (false? (higher-kicker? [] [])))))


(deftest winning-hand-test
  (testing "winning-hand function"
    (is (= straight-flush-hand (winning-hand [high-seven pair-hand straight-flush-hand])))
    (is (= four-of-a-kind-hand (winning-hand [pair-hand four-of-a-kind-hand three-of-a-kind-hand])))
    (is (nil? (winning-hand [])))
    (is (= full-house-hand (winning-hand [pair-hand two-pairs-hand full-house-hand])))))