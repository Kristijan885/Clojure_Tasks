(ns boja-test
  (:require [clojure.test :refer :all]
            [boja :refer :all]))

(deftest rotate-colors-test
  (testing "rotate-colors"
    (is (= '(:yellow :red :blue :green)
           (rotate-colors '(:red :blue :green :yellow))))

    (is (= '(:green :red :blue)
           (rotate-colors '(:red :blue :green))))

    (is (= '(:blue :red)
           (rotate-colors '(:red :blue))))

    (is (= '(:red)
           (rotate-colors '(:red))))

    (is (= '()
           (rotate-colors '())))))

(deftest unique-colors-in-column-test
  (testing "unique-colors-in-column"
    (is (true? (unique-colors-in-column?
                 '((:red :blue)
                   (:blue :red)))))

    (is (false? (unique-colors-in-column?
                  '((:red :blue)
                    (:red :green)))))

    (is (true? (unique-colors-in-column?
                 '((:red :blue :green)
                   (:blue :green :red)
                   (:green :red :blue)))))

    (is (true? (unique-colors-in-column? '())))

    (is (true? (unique-colors-in-column?
                 '((:red :blue :green)))))))

(deftest arrangement-test
  (testing "arrange-colors"
    (let [input1 '((:red :blue)
                   (:blue :red))
          result1 (arrange-colors input1)]
      (is (not (nil? result1)))
      (is (unique-colors-in-column? result1)))

    (let [input2 '((:red :blue :green)
                   (:green :red :blue)
                   (:blue :green :red))
          result2 (arrange-colors input2)]
      (is (not (nil? result2)))
      (is (unique-colors-in-column? result2)))

    (let [input3 '((:red :red)
                   (:red :red))
          result3 (arrange-colors input3)]
      (is (nil? result3)))

    (let [input4 '((:red :blue :green :yellow)
                   (:blue :red :yellow :green)
                   (:blue :red :green :yellow))
          result4 (arrange-colors input4)]
      (is (not (nil? result4)))
      (is (unique-colors-in-column? result4)))

    (let [input5 '()
          result5 (arrange-colors input5)]
      (is (= '() result5)))

    (let [input6 '((:red :blue :green :yellow)
                   (:yellow :red :blue :green)
                   (:green :yellow :red :blue)
                   (:blue :green :yellow :red))
          result6 (arrange-colors input6)]
      (is (not (nil? result6)))
      (is (unique-colors-in-column? result6)))))

(defn test-ns-hook []
  (rotate-colors-test)
  (unique-colors-in-column-test)
  (arrangement-test))