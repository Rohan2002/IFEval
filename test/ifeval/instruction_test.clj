(ns ifeval.instruction-test
  (:require [clojure.test :refer :all]
            [ifeval.instructions :refer :all]))

(deftest test-ifeval-rule1
  (testing "IFeval rule 1. Keywords that exist in Corpus of text."
    (is (= (keywords-contain? "This is b." ["is"]) #{"is"}))))
(deftest test-ifeval-rule2
  (testing "IFeval rule 2. Text contains the keyword" \
    (is (keywords-freq? "The cat is black bot" "black" 1)))
  (testing "IFeval rule 2. Text does not contain the keyword" \
      (is (keywords-freq? "The cat is black bot" "f" 0))))

(deftest test-ifeval-rule3
  (testing "IFeval rule 3. Text has 2 forbidden words" \
    (is (keywords-forbidden? "The cat is black bot" ["black" "bot"] 2)))
  (testing "IFeval rule 3. Text has 1 forbidden words" \
      (is (keywords-forbidden? "The cat is black bot" ["none" "bot"] 1)))
  (testing "IFeval rule 3. Text has 0 forbidden words" \
      (is (keywords-forbidden? "The cat is black bot" ["none1" "none2" "none3"] 0))))

(deftest test-ifeval-rule4
  (testing "IFeval rule 4. Text has 2 b characters" \
    (is (keywords-character-freq? "The cat is black bot" "b" 2))))

(deftest test-ifeval-rule5
  (testing "IFeval rule 5. English detection" \
    (is (language-detect? "The cat is black" "en")))
  (testing "IFeval rule 5. French detection" \
    (is (language-detect? "Bien sûr! Voici une phrase en français" "fr")))
  (testing "IFeval rule 5. Japanese detection" \
    (is (language-detect? "もちろんです" "ja"))))

(deftest test-ifeval-rule6
  (testing "IFeval rule 6. Check if corpus has 4 paragraphs" \
    (is (num-paragraphs? "a***b***c***d" 4))))

(deftest test-ifeval-rule7
  (testing "IFeval rule 7. Check if corpus has exactly {N} words."
    (is (num-words? "This is A. This is B." "at least" 6)))
  (testing "IFeval rule 7. Check if corpus has at least {N} words."
    (is (num-words? "This is A. This is B." "at least" 3)))
  (testing "IFeval rule 7. Check if corpus has at most {N} words."
    (is (num-words? "This is A. This is B." "at most" 100)))
  (testing "IFeval rule 7. Check if corpus has less than {N} words."
    (is (not (num-words? "This is A. This is B." "less than" 1))))
  (testing "IFeval rule 7. Check if corpus has more than {N} words."
    (is (not (num-words? "This is A. This is B." "more than" 6)))))

(deftest test-ifeval-rule8
  (testing "IFeval rule 8. Check if corpus has exactly 2 sentences."
    (is (num-sentences? "This is A. This is B." "exactly" 2)))
  (testing "IFeval rule 8. Check if corpus has at least 2 sentences."
    (is (num-sentences? "This is A. This is B." "at least" 2)))
  (testing "IFeval rule 8. Check if corpus has at most {N} sentences."
    (is (num-sentences? "This is A. This is B." "at most" 2)))
  (testing "IFeval rule 8. Check if corpus has less than {N} sentences."
    (is (not (num-sentences? "This is A. This is B." "less than" 2))))
  (testing "IFeval rule 8. Check if corpus has more than {N} sentences."
    (is (not (num-sentences? "This is A. This is B." "more than" 2)))))

(deftest test-ifeval-rule9
  (testing "IFeval rule 9. Check if corpus has 3 paragraph and 1st paragraph starts with This"
    (is (num-paragraphs-with-starting-word? "This is A.\n\nThat is B.\n\nThose is C." 3 1 "This")))
  (testing "IFeval rule 9. Check if corpus has 3 paragraph and 2nd paragraph starts with That"
    (is (num-paragraphs-with-starting-word? "This is A.\n\nThat is B.\n\nThose is C." 3 2 "That")))
  (testing "IFeval rule 9. Check if corpus has 3 paragraph and 3rd paragraph starts with Those"
    (is (num-paragraphs-with-starting-word? "This is A.\n\nThat is B.\n\nThose is C." 3 3 "Those")))
  (testing "IFeval rule 9. Check if corpus has 0 paragraph and 3rd paragraph starts with Those"
    (is (not (num-paragraphs-with-starting-word? "" 0 3 "Those")))))

(deftest test-ifeval-rule10
  (testing "IFeval rule 10. Check if postscript P.P.S exist at the end of the response"
    (is (postscript-detect? "This is A.\n\nThat is B.\n\nP.P.S Those is C." "P.P.S"))))