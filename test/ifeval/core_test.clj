(ns ifeval.core-test
  (:require [clojure.test :refer :all]
            [ifeval.core :refer :all]))

(deftest test-loose-comparsion
  []
  (testing "Testing loose-comparsion"
    (is (loose-comparsion [true true true]))
    (is (not (loose-comparsion [false false false])))
    (is (loose-comparsion [true false true]))
    (is (not (loose-comparsion [false false false false])))))

(deftest test-strict-comparsion
  []
  (testing "Testing strict-comparsion"
    (is (strict-comparsion [true true true]))
    (is (not (strict-comparsion [false false false])))
    (is (not (strict-comparsion [true false true])))
    (is (not (strict-comparsion [false false false false])))))

(deftest test-apply-instruction-function
  []
  (testing "Testing apply instruction (instruction, no arguement)"
    (let [instruction ["punctuation:no_comma" {}]
          corpus "This is a text without any comma."]
      (is (= (apply-instruction-function corpus instruction) true))))
  (testing "Testing apply instruction (instruction, with arguement)"
    (let [instruction ["length_constraints:number_words" {:relation "exactly" :num_words 7}]
          corpus "This is a text has 7 words."]
      (is (= (apply-instruction-function corpus instruction) true)))))