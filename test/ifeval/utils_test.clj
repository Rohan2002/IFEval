(ns ifeval.utils-test
  (:require [ifeval.utils :as util]
            [clojure.test :refer :all]))

(deftest test-replace-punctuation-with-space
  []
  (testing "Testing replace-punctuation-with-space"
    (let [input "Hello, world! How are you?"
          expected "Hello world How are you"]
      (is (= expected (util/replace-punctuation-with-space input))))))

(deftest test-get-words-from-corpus
  []
  (testing "Testing get-words-from-corpus"
    (let [input "Hello, this is Rohan!"
          expected ["Hello" "this" "is" "Rohan"]]
      (is (= expected (util/get-words-from-corpus input))))))

(deftest test-get-sentences-from-corpus
  []
  (testing "Testing get-sentences-from-corpus"
    (let [input "This is A. This is B."
          expected ["This is A." "This is B."]]
      (is (= expected (util/get-sentences-from-corpus input))))))

(deftest test-get-paragraph-from-corpus
  []
  (testing "Testing get-paragraph-from-corpus 1"
    (let [input "This is A.\n\nThis is B."
          expected ["This is A." "This is B."]]
      (is (= expected (util/get-paragraph-from-corpus input #"\n\n"))))))

(deftest number-of-keyword-in-corpus
  []
  (testing "Testing number-of-keyword-in-corpus"
    (let [input "This is A and A and A"
          expected 3]
      (is (= expected (util/number-of-keyword-in-corpus input "A"))))))