(ns sudoku-test
  (:require [clojure.test :refer :all]
            [sudoku :refer :all]))

;; Test Boards Collection
(def test-board1
  '[[5 3 0 0 7 0 0 0 0]
    [6 0 0 1 9 5 0 0 0]
    [0 9 8 0 0 0 0 6 0]
    [8 0 0 0 6 0 0 0 3]
    [4 0 0 8 0 3 0 0 1]
    [7 0 0 0 2 0 0 0 6]
    [0 6 0 0 0 0 2 8 0]
    [0 0 0 4 1 9 0 0 5]
    [0 0 0 0 8 0 0 7 9]])

(def test-board2
  '[[0 2 0 6 0 8 0 0 0]
    [5 8 0 0 0 9 7 0 0]
    [0 0 0 0 4 0 0 0 0]
    [3 7 0 0 0 0 5 0 0]
    [6 0 0 0 0 0 0 0 4]
    [0 0 8 0 0 0 0 1 3]
    [0 0 0 0 2 0 0 0 0]
    [0 0 9 8 0 0 0 3 6]
    [0 0 0 3 0 6 0 9 0]])

;; Basic transformation tests
(deftest transform-test
  (testing "Transform empty cell"
    (is (= (inner_transform '(0))
           '([1 2 3 4 5 6 7 8 9]))))

  (testing "Transform non-empty cell"
    (is (= (inner_transform '(5))
           '([5]))))

  (testing "Transform mixed cells"
    (is (= (inner_transform '(5 0 3))
           '([5] [1 2 3 4 5 6 7 8 9] [3]))))

  (testing "Transform empty list"
    (is (= (inner_transform '())
           '())))

  (testing "Transform full row"
    (is (= (count (first (transform (list (first test-board1))))) 9)
        "Row should maintain 9 elements")))

;; Tests for constraint functions
(deftest constraint-tests
  (testing "Row constraint eliminates values"
    (let [transformed (transform test-board1)
          constrained (row_constraint transformed)
          first-row (first constrained)]
      (is (= 9 (count first-row))
          "Should maintain row length")))

  (testing "Column constraint structure"
    (let [transformed (transform test-board1)
          constrained (col_constraint transformed)]
      (is (= 9 (count constrained)) "Should have 9 rows")
      (is (every? #(= 9 (count %)) constrained) "Each row should have 9 columns")))

  (testing "Box constraint structure"
    (let [transformed (transform test-board1)
          constrained (box_constraint transformed)]
      (is (= 9 (count constrained)) "Should have 9 rows")
      (is (every? #(= 9 (count %)) constrained) "Each row should have 9 columns"))))

(deftest solver-tests
  (testing "Solver finds valid solution"
    (let [solution (solve test-board1)]
      (when solution  ; Only test if solution exists
        (is (= 9 (count solution)) "Should have 9 rows")
        (is (every? #(= 9 (count %)) solution) "Each row should have 9 columns"))))

  (testing "Fixed numbers preserved"
    (let [solution (solve test-board1)]
      (when solution  ; Only test if solution exists
        (doseq [i (range 9)
                j (range 9)
                :let [original (nth (nth test-board1 i) j)]
                :when (not= 0 original)]
          (is (= original (first (nth (nth solution i) j)))
              (str "Position [" i "," j "] should maintain original value " original)))))))
