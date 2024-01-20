(ns ifeval.instruction-test
  (:require [clojure.test :refer :all]
            [ifeval.instructions :refer :all]))

(deftest test-ifeval-rule1
  (testing "IFeval rule 1. Keywords that exist in Corpus of text."
    (is (= (keywords-contain? "This is b." :keywords ["is"]) #{"is"}))))

(deftest test-ifeval-rule2
  (testing "IFeval rule 2. Text contains the keyword" \
    (is (keywords-freq? "The cat is black bot" :relation "exactly" :keyword "black" :frequency 1)))
  (testing "IFeval rule 2. Text does not contain the keyword" \
      (is (keywords-freq? "The cat is black bot" :relation "exactly" :keyword "f" :frequency 0))))

(deftest test-ifeval-rule3
  (testing "IFeval rule 3. Text has 2 forbidden words" \
    (is (keywords-forbidden? "The cat is black bot" :forbidden_words ["black" "bot"])))
  (testing "IFeval rule 3. Text has 1 forbidden words" \
      (is (keywords-forbidden? "The cat is black bot" :forbidden_words ["none" "bot"])))
  (testing "IFeval rule 3. Text has 0 forbidden words" \
      (is (not (keywords-forbidden? "The cat is black bot" :forbidden_words ["none1" "none2" "none3"])))))

(deftest test-ifeval-rule4
  (testing "IFeval rule 4. Text has 2 b characters" \
    (is (keywords-character-freq? "The cat is black bot" :let_relation "exactly" :letter "b" :let_frequency 2))))

(deftest test-ifeval-rule5
  (testing "IFeval rule 5. English detection" \
    (is (language-detect? "The cat is black" :language "en")))
  (testing "IFeval rule 5. French detection" \
    (is (language-detect? "Bien sûr! Voici une phrase en français" :language "fr")))
  (testing "IFeval rule 5. Japanese detection" \
    (is (language-detect? "もちろんです" :language "ja"))))

(deftest test-ifeval-rule6
  (testing "IFeval rule 6. Check if corpus has 4 paragraphs" \
    (is (num-paragraphs? "a***b***c***d" :num_paragraphs 4))))

(deftest test-ifeval-rule7
  (testing "IFeval rule 7. Check if corpus has exactly {N} words."
    (is (num-words? "This is A. This is B." :relation "at least" :num_words 6)))
  (testing "IFeval rule 7. Check if corpus has at least {N} words."
    (is (num-words? "This is A. This is B." :relation "at least" :num_words 3)))
  (testing "IFeval rule 7. Check if corpus has at most {N} words."
    (is (num-words? "This is A. This is B." :relation "at most" :num_words 100)))
  (testing "IFeval rule 7. Check if corpus has less than {N} words."
    (is (not (num-words? "This is A. This is B." :relation "less than" :num_words 1))))
  (testing "IFeval rule 7. Check if corpus has more than {N} words."
    (is (not (num-words? "This is A. This is B." :relation "more than" :num_words 6)))))

(deftest test-ifeval-rule8
  (testing "IFeval rule 8. Check if corpus has exactly 2 sentences."
    (is (num-sentences? "This is A. This is B." :relation "exactly" :num_sentences 2)))
  (testing "IFeval rule 8. Check if corpus has at least 2 sentences."
    (is (num-sentences? "This is A. This is B." :relation "at least" :num_sentences 2)))
  (testing "IFeval rule 8. Check if corpus has at most {N} sentences."
    (is (num-sentences? "This is A. This is B." :relation "at most" :num_sentences 2)))
  (testing "IFeval rule 8. Check if corpus has less than {N} sentences."
    (is (not (num-sentences? "This is A. This is B." :relation "less than" :num_sentences 2))))
  (testing "IFeval rule 8. Check if corpus has more than {N} sentences."
    (is (not (num-sentences? "This is A. This is B." :relation "more than" :num_sentences 2)))))

(deftest test-ifeval-rule9
  (testing "IFeval rule 9. Check if corpus has 3 paragraph and 1st paragraph starts with This"
    (is (num-paragraphs-with-starting-word? "This is A.\n\nThat is B.\n\nThose is C." :num_paragraphs 3 :nth_paragraph 1 :first_word "This")))
  (testing "IFeval rule 9. Check if corpus has 3 paragraph and 2nd paragraph starts with That"
    (is (num-paragraphs-with-starting-word? "This is A.\n\nThat is B.\n\nThose is C." :num_paragraphs 3 :nth_paragraph 2 :first_word "That")))
  (testing "IFeval rule 9. Check if corpus has 3 paragraph and 3rd paragraph starts with Those"
    (is (num-paragraphs-with-starting-word? "This is A.\n\nThat is B.\n\nThose is C." :num_paragraphs 3 :nth_paragraph 3 :first_word "Those")))
  (testing "IFeval rule 9. Check if corpus has 0 paragraph and 3rd paragraph starts with Those"
    (is (not (num-paragraphs-with-starting-word? "" :num_paragraphs 0 :nth_paragraph 3 :first_word "Those")))))

(deftest test-ifeval-rule10
  (testing "IFeval rule 10. Check if postscript P.P.S exist at the end of the response"
    (is (postscript-detect? "This is A.\n\nThat is B.\n\nP.P.S Those is C." :postscript_marker "P.P.S"))))

(deftest test-ifeval-rule11
  (testing "IFeval rule 11. Check if corpus has at least 5 [text] placeholders"
    (is (placeholders-detect? "This is [text]. He is [text]. [text] [text] [text]" :num_placeholders 5)))
  (testing "IFeval rule 11. Check if corpus does not have at least 6 placeholders"
    (is (not (placeholders-detect? "This is [text]. He is [text]. [a] [b] [text]" :num_placeholders 6))))
  (testing "IFeval rule 11. Check if corpus does not have at least 3 (only 2. [] is not considered a valid placeholder) placeholders"
    (is (not (placeholders-detect? "This is [a]. He is [b]. []" :num_placeholders 3)))))

(deftest test-ifeval-rule13
  (testing "IFeval rule 11. Check if corpus has a title."
    (is (contain-title? "This is <<title one>>.")))
  (testing "IFeval rule 11. Check if corpus has no title."
    (is (not (contain-title? "This is <<title one>>>."))))
  (testing "IFeval rule 11. Check if corpus has no title 2."
    (is (not (contain-title? "This is foo.")))))

(deftest test-ifeval-rule20
  (testing "Check if all-uppercase? works correctly true case."
    (is (all-uppercase? "THIS IS A.")))
  (testing "Check if all-uppercase? works correctly false case."
    (is (not (all-uppercase? "this is A.")))))

(deftest test-ifeval-rule21
  (testing "Check if all-lowercase? works correctly true case."
    (is (all-lowercase? "this is a.")) 
    )
  (testing "Check if all-lowercase? works correctly false case."
    (is (not (all-lowercase? "This is a.")))) 
  )

(deftest test-ifeval-rule22
  (testing "Check if freq-all-capital-words? works correctly 1"
    (is (freq-all-capital-words? "HELLO WORLD HELLO CLOJURE" :capital_relation "at most" :capital_frequency 2)))
  (testing "Check if freq-all-capital-words? works correctly 2"
    (is (freq-all-capital-words? "This IS A and b." :capital_relation "exactly" :capital_frequency 2)))
  (testing "Check if freq-all-capital-words? works correctly"
    (is (freq-all-capital-words? "This is b." :capital_relation "at most" :capital_frequency 0))) 
  )

(deftest test-ifeval-rule23
  (testing "IFeval rule 23. Check string ends with a certain phrase. Test 1")
  (is (ends-with-response? "This is A. This is B." :end_phrase "This is B."))
  (testing "IFeval rule 23. Check string ends with a certain phrase. Test 2")
  (is (ends-with-response? "This is B." :end_phrase "This is B."))
  (testing "IFeval rule 23. Check string NOT ends with a certain phrase. Test 3")
  (is (not (ends-with-response? "" :end_phrase "This is B."))))


(deftest test-ifeval-rule24
  (testing "IFeval rule 24. Check string starts and ends with \"")
  (is (wrap-double-quotes? "\"This is foo.\""))
  (testing "IFeval rule 24. Check string does not start and end with \"")
  (is (not (wrap-double-quotes? "\"This is foo,")))
  (testing "IFeval rule 24. Check string does not start and end with \"")
  (is (not (wrap-double-quotes? "This is foo,"))))

(deftest test-ifeval-rule25
  (testing "IFeval rule 25. Check no commas exist in string")
  (is (no-commas? "This is foo."))
  (testing "IFeval rule 25. Check commas exist in string")
  (is (not (no-commas? "This is foo,"))))