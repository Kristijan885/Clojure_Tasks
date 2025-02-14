(ns Task_1-test
  (:require [clojure.test :refer :all]
            [Task_1 :refer :all]))

(deftest test-atomic
  (testing "atomic function tests"
    (is (= false (atomic '[4])))
    (is (= true (atomic 'a)))
    (is (= true (atomic '5)))
    (is (= true (atomic '%)))
    (is (= false (atomic '[a, 3])))))

(deftest test-member
  (testing "member function tests"
    (is (= true (member 5 '[1 2 3 4 5])))
    (is (= false (member 'a '[])))
    (is (= true (member 'x '[x y z])))
    (is (= false (member 7 '[1 2 3])))
    (is (= true (member 'b '[a b c])))))

(deftest test-my-count
  (testing "my-count function tests"
    (is (= 3 (my-count '(1 (2 3) 4 5))))
    (is (= 3 (my-count '(a b (c d e) f (g h)))))
    (is (= 0 (my-count '((1 2) (3 4) (5 6)))))
    (is (= 3 (my-count '(x y z))))
    (is (= 0 (my-count '())))))

(deftest test-append
  (testing "append function tests"
    (is (= '(:a :b :c 1 (2 3)) (append '(:a :b :c) '(1 (2 3)))))
    (is (= '(1 2 3) (append '() '(1 2 3))))
    (is (= '(1 2 3) (append '(1 2 3) '())))
    (is (= '(a b c d) (append '(a b) '(c d))))
    (is (= '((1 2) 3 4 5) (append '((1 2) 3) '(4 5))))))

(deftest test-zip
  (testing "zip function tests"
    (is (= [[1 :a] [2 :b] [3 :c]] (zip [1 2 3] [:a :b :c])))
    (is (= [[1 :a] [2 :b]] (zip [1 2 3] [:a :b])))
    (is (= [[1 :a] [2 :b]] (zip [1 2] [:a :b :c])))
    (is (= [] (zip [] [1 2 3])))
    (is (= [] (zip [1 2 3] [])))))


(deftest test-lookup
  (testing "lookup function tests"
    (is (= 1 (lookup 'a '((a 1) (b 2)))))
    (is (= 2 (lookup '(1 2) '(((1 2) 2) ((3 4) 4)))))
    (is (= nil (lookup 'c '((a 1) (b 2)))))
    (is (= 5 (lookup '(x y) '(((x y) 5) (a 1)))))
    (is (= nil (lookup '(1 2) '())))))

(deftest test-merge
  (testing "merge function tests"
    (is (= '(1 2 3 4 5 6) (my-merge '(1 3 5) '(2 4 6))))
    (is (= '(1 2 3) (my-merge '(1 2 3) '())))
    (is (= '(1 1 2 2 3 3) (my-merge '(1 2 3) '(1 2 3))))
    (is (= '(1 2 3 4 5) (my-merge '(1 3 5) '(2 4))))
    (is (= '(-3 -2 -1 0 1 2) (my-merge '(-3 -1 1) '(-2 0 2))))))

(deftest test-count-all
  (testing "count-all function tests"
    (is (= 3 (count-all '(1 2 3))))
    (is (= 6 (count-all '(1 (2 3) (4 5 6)))))
    (is (= 4 (count-all '((1 2) (3 4)))))
    (is (= 5 (count-all '(1 (2 (3 (4) 5))))))
    (is (= 0 (count-all '())))))

(deftest test-drop
  (testing "my-drop function tests"
    (is (= '(4 5) (my-drop 3 '(1 2 3 4 5))))
    (is (= '() (my-drop 3 '(1 2 3))))
    (is (= '() (my-drop 5 '(1 2 3))))
    (is (= '(a b c) (my-drop 0 '(a b c))))
    (is (= '(2 3) (my-drop 1 '(1 2 3))))))

(deftest test-take
  (testing "my-take function tests"
    (is (= '(1 2 3) (my-take 3 '(1 2 3 4 5))))
    (is (= '(1 2 3) (my-take 5 '(1 2 3))))
    (is (= '() (my-take 0 '(1 2 3 4 5))))
    (is (= '(a b) (my-take 2 '(a b c d))))
    (is (= '(1) (my-take 1 '(1 2 3))))))

(deftest test-reverse
  (testing "my-reverse function tests"
    (is (= '(3 2 1) (my-reverse '(1 2 3))))
    (is (= '() (my-reverse '())))
    (is (= '(a) (my-reverse '(a))))
    (is (= '(d c b a) (my-reverse '(a b c d))))
    (is (= '(5 4 3 2 1) (my-reverse '(1 2 3 4 5))))))

(deftest test-duplicates
  (testing "remove-duplicates function tests"
    (is (= '(1 2 3) (remove-duplicates '(1 1 1 2 2 3))))
    (is (= '(a b c) (remove-duplicates '(a a b c c c))))
    (is (= '(x) (remove-duplicates '(x x x x x))))
    (is (= '() (remove-duplicates '())))
    (is (= '(1 2 3 4) (remove-duplicates '(1 2 2 3 3 3 4))))))

(deftest test-flatten
  (testing "my-flatten function tests"
    (is (= '((1 1) (2 3) (5 7))
           (my-flatten '(((1 1) (2 3)) ((5 7))))))
    (is (= '(1 2 3)
           (my-flatten '((1) (2) (3)))))
    (is (= '()
           (my-flatten '())))
    (is (= '((1 2) (3 4))
           (my-flatten '(((1 2)) ((3 4))))))
    (is (= '((a b) (c d) (e f))
           (my-flatten '(((a b)) ((c d) (e f))))))))

(deftest test-buzz
  (testing "buzz function tests"
    (is (= '(1 2 3 4 :buzz)
           (buzz '(1 2 3 4 5))))
    (is (= '(1 2 3 4 :buzz :buzz 7 8 9 :buzz)
           (buzz '(1 2 3 4 5 15 7 8 9 10))))
    (is (= '(:buzz 2 3 4 :buzz)
           (buzz '(51 2 3 4 50))))
    (is (= '()
           (buzz '())))
    (is (= '(:buzz :buzz :buzz)
           (buzz '(52 55 25))))))

(deftest test-divisors
  (testing "divisors function tests"
    (is (= '(2 3 4 6) (divisors-of 12)))
    (is (= '(2 4 8) (divisors-of 16)))
    (is (= '(2 3 6 9) (divisors-of 18)))
    (is (= '(2 5) (divisors-of 10)))
    (is (= '(2 3 4 6 8 12) (divisors-of 24)))))

(deftest test-longest
  (testing "longest list function tests"
    (is (= '(1 2 3 4)
           (longest '((1) (1 2) (1 2 3) (1 2 3 4)))))
    (is (= '(a b c d e)
           (longest '((a b) (a b c) (a b c d e)))))
    (is (= '(1 2 3 4 5)
           (longest '((1 2) (1 2 3 4 5) (1 2 3)))))
    (is (= '(x y z)
           (longest '((x y z) (a b e) (m n)))))
    (is (= '(5 5 5 5)
           (longest '((5) (5 5) (5 5 5) (5 5 5 5)))))))

(deftest test-my-map
  (testing "my-map function tests"
    (is (= '(2 3 4)
           (my-map inc '(1 2 3))))

    (is (= '(2 4 6 8)
           (my-map #(* % 2) '(1 2 3 4))))

    (is (= '("hello!" "world!")
           (my-map #(str % "!") '("hello" "world"))))

    (is (= '(false true false true)
           (my-map even? '(1 2 3 4))))

    (is (= '()
           (my-map inc '())))))


(deftest test-filter
  (testing "my-filter function tests"
    (is (= '(2 4 6)
           (my-filter even? '(1 2 3 4 5 6))))

    (is (= '(1 3 5)
           (my-filter #(= 1 (mod % 2)) '(1 2 3 4 5 6))))

    (is (= '("apple" "avocado")
           (my-filter #(.startsWith % "a") '("apple" "banana" "avocado"))))

    (is (= '(4 5 6)
           (my-filter #(> % 3) '(1 2 3 4 5 6))))

    (is (= '()
           (my-filter pos? '(-1 -2 -3))))))


(deftest test-reduce
  (testing "my-reduce function tests"
    (is (= 10
           (my-reduce + nil '(1 2 3 4))))

    (is (= 24
           (my-reduce * nil '(2 3 4))))

    (is (= 15
           (my-reduce + 5 '(1 2 3 4))))

    (is (= "hello world !"
           (my-reduce str "hello" '(" world" " !"))))

    (is (= 1
           (my-reduce min 10 '(5 3 1 7))))))

(deftest test-flat-map
  (testing "my-flat-map function tests"
    (is (= '(1 2 3 4 5 6)
           (my-flat-map (fn [x] (if (list? x) x (list x))) '((1 2) 3 (4 5 6)))))

    (is (= '(a b c d)
           (my-flat-map (fn [x] (if (list? x) x (list x))) '((a b) (c d)))))

    (is (= '(1 2 3)
           (my-flat-map (fn [x] (list x)) '(1 2 3))))

    (is (= '()
           (my-flat-map (fn [x] (list x)) '())))

    (is (= '(1 2 3 4 5)
           (my-flat-map (fn [x] (if (list? x) x (list x))) '(1 (2 3) (4 5)))))))

(run-tests)