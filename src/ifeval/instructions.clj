(ns ifeval.instructions
  (:require [clojure.string :as string]
            [clojure.set :as s]
            [ifeval.utils :as utils]))

(defn keywords-contain?
  "Return the keywords that exist in the corpus of text (IFeval rule 1) (Include Keywords)"
  [corpus & {:keys [keywords]}]
  (->>
   (s/intersection (set (utils/get-words-from-corpus corpus)) (set (map name keywords)))
   (empty?)
   (not)))

(defn keywords-freq?
  "Check if keyword appears n times in corpus (IFeval rule 2) (Keyword Frequency)"
  [corpus & {:keys [relation keyword frequency]}]
  (->
   (frequencies (utils/get-words-from-corpus corpus))
   (get keyword 0)
   ((get utils/function-map relation "exactly") frequency)))

(defn keywords-forbidden?
  "Check if at least one keyword from keywords appears in corpus (IFeval rule 3) (Forbidden Words)"
  [corpus & {:keys [forbidden_words]}]
  #_{:clj-kondo/ignore [:not-empty?]}
  (keywords-contain? corpus :keywords forbidden_words))

(defn keywords-character-freq?
  "Check if corpus has n number of characters (IFeval rule 4) (Letter Frequency)"
  [corpus & {:keys [let_relation letter let_frequency]}]
  (->
   (frequencies corpus)
   (get (.charAt letter 0) 0)
   ((get utils/function-map let_relation "exactly") let_frequency)))

(defn language-detect?
  "Detect language of text (IFEval rule 5) (Response Language)"
  [corpus & {:keys [language]}]
  (= language (utils/language-detect? corpus)))

(defn num-paragraphs?
  "Check if corpus has exactly n paragraphs. Paragraphs are seperated by markdown divider *** (IFeval rule 6) (Number Paragraphs)"
  [corpus & {:keys [num_paragraphs]}]
  (->
   (count (utils/get-paragraph-from-corpus corpus #"\*\*\*"))
   (= num_paragraphs)))

(defn num-words?
  "Check if corpus has \"at least\", \"at most\", \"less than\", \"more than\", \"exactly\" n words (IFEval rule 7) (Number Words)"
  [corpus & {:keys [relation num_words]}]
  (->
   (count (utils/get-words-from-corpus corpus))
   ((get utils/function-map relation "exactly") num_words)))

(defn num-sentences?
  "Check if corpus has \"at least\", \"at most\", \"less than\", \"more than\", \"exactly\" n sentences (IFEval rule 8) (Number Sentences)"
  [corpus & {:keys [relation num_sentences]}]
  (->
   (count (utils/get-sentences-from-corpus corpus))
   ((get utils/function-map relation "exactly") num_sentences)))

(defn num-paragraphs-with-starting-word?
  "There should be {N} paragraphs. 
   Paragraphs and only paragraphs are separated with each other by two line breaks. 
   The {i}-th paragraph must start with word {first word}.
   (IFEval rule 9) (Number Paragraphs + First Word in i-th Paragraph)"
  [corpus & {:keys [first_word num_paragraphs nth_paragraph]}]
  (let [paragraphs (utils/get-paragraph-from-corpus corpus #"\n\n")
        first-word-function (comp first utils/get-words-from-corpus)]
    (let [first-word-of-paragraphs (map first-word-function paragraphs)
          nth-first-word (nth first-word-of-paragraphs (- nth_paragraph 1) 0)]
      (and (= (count first-word-of-paragraphs) num_paragraphs) (= nth-first-word first_word)))))

(defn postscript-detect?
  "At the end of your response, please explicitly add a postscript starting with {postscript marker} (IFEval rule 10) (Postscript)"
  [corpus & {:keys [postscript_marker]}]
  (->
   (last (utils/get-paragraph-from-corpus corpus #"\n\n"))
   (string/starts-with? postscript_marker)))


(defn placeholders-detect?
  "The response must contain at least {N} placeholders represented by square brackets, such as [address]. (IFEval rule 11) (Number Placeholder)"
  [corpus & {:keys [num_placeholders]}]
  (->
   (count (re-seq #"\[.+?\]" corpus))
   ((get utils/function-map "at least") num_placeholders)))

(defn bullets-detect?
  "Your answer must contain exactly {N} bullet points. Use the markdown bullet points such as: * This is a point. (IFEval rule 12) (Number Bullets)"
  [corpus & {:keys [num_bullets]}]
  (->>
   (utils/number-of-keyword-in-corpus corpus "*")
   ((get utils/function-map "exactly") num_bullets)))

(defn contain-title?
  "Your answer must contain a title, wrapped in double angular brackets, such as <<poem of joy>>. (IFEval rule 13) (Title)"
  [corpus]
  (->
   (re-find #"[^<]<<([^>>]+)>>[^>]" corpus)
   (count)
   ((get utils/function-map "at least") 1)))

(defn choose-from
  "Answer with one of the following options: {options}. (IFEval rule 14) (Choose From)"
  [corpus]
  (= 1 1))

(defn highlight-n-sections?
  "Highlight at least {N} sections in your answer with markdown, i.e. *highlighted section* (IFeval rule 15) (Min highlighted sections)"
  [corpus & {:keys [num_highlights]}]
  (= 1 1))

(defn multiple-sections?
  "Your response must have {N} sections. Mark the beginning of each section with {section splitter} X. (IFeval rule 16) (Multiple sections)"
  [corpus & {:keys [section_spliter num_sections]}]
  (= 1 1))

(defn is-json?
  "Is response in json format? (IFEval rule 17) (JSON Format)"
  [corpus]
  (= 1 1))

(defn repeat-request?
  "First, repeat the request without change, then give your answer (do not say anything before repeating the request; the request you need to 
   repeat does not include this sentence) 
   (IFeval rule 18) (Repeat Prompt)"
  [corpus & {:keys [prompt_to_repeat]}]
  (= 1 1))

(defn two-responses?
  "Give two different responses. Responses and only responses should be separated by 6 asterisk symbols: ******. (IFeval rule 19) (Two Responses)"
  [corpus]
  (let [spl (string/split corpus #"\*\*\*\*\*\*")]
    (and (= (count spl) 2) (not= (nth spl 0) (nth spl 1)))))

(defn all-uppercase?
  "Your entire response should be in English, capital letters only. (IFeval rule 20) (All Uppercase)"
  [corpus]
  (utils/all-uppercase? corpus))

(defn all-lowercase?
  "Your entire response should be in English, and in all lowercase letters. No capital letters are allowed. (IFeval rule 21) (All Lowecase)"
  [corpus]
  (utils/all-lowercase? corpus))

(defn freq-all-capital-words?
  "In your response, words with all capital letters should appear at least / around / at most {N} times (IFeval rule 22) (Frequency of All Capital Words)"
  [corpus & {:keys [capital_relation capital_frequency]}]
  (->>
   (utils/get-words-from-corpus corpus)
   (filter #(utils/all-uppercase? %))
   (count)
   ((get utils/function-map capital_relation "exactly") capital_frequency)))

(defn ends-with-response?
  "Finish your response with this exact phrase {end phrase}. No other words should follow this phrase. (IFeval rule 23) (End Checker)"
  [corpus & {:keys [end_phrase]}]
  (let [len_corpus (count corpus)
        len_end (count end_phrase)]
    (if (or (= len_corpus 0) (= len_end 0))
      (= 1 0) ;; always false
      (= (subs corpus (- len_corpus len_end) len_corpus) end_phrase))))

(defn wrap-double-quotes?
  "Wrap your entire response with double quotation marks. (IFeval rule 24) (Quotation)"
  [corpus]
  (and (string/starts-with? corpus "\"") (string/ends-with? corpus "\"")))

(defn no-commas?
  "In your entire response, refrain from the use of any commas. (IFeval rule 25) (No Commas)"
  [corpus]
  (= nil (string/index-of corpus ",")))

(def mp-ins-func
  {"keywords:existence" {:function keywords-contain?, :rule-number 1}
   "keywords:frequency" {:function keywords-freq?, :rule-number 2}
   "keywords:forbidden_words" {:function keywords-forbidden?, :rule-number 3}
   "keywords:letter_frequency" {:function keywords-character-freq?, :rule-number 4}
   "language:response_language" {:function language-detect?, :rule-number 5}
   "length_constraints:number_paragraphs" {:function num-paragraphs?, :rule-number 6}
   "length_constraints:number_words" {:function num-words?, :rule-number 7}
   "length_constraints:number_sentences" {:function num-sentences?, :rule-number 8}
   "length_constraints:nth_paragraph_first_word" {:function num-paragraphs-with-starting-word?, :rule-number 9}
   "detectable_content:postscript" {:function postscript-detect?, :rule-number 10}
   "detectable_content:number_placeholders" {:function placeholders-detect?, :rule-number 11}
   "detectable_format:number_bullet_lists" {:function bullets-detect?, :rule-number 12}
   "detectable_format:title" {:function contain-title?, :rule-number 13}
   "detectable_format:constrained_response" {:function choose-from, :rule-number 14}
   "detectable_format:number_highlighted_sections" {:function highlight-n-sections?, :rule-number 15}
   "detectable_format:multiple_sections" {:function multiple-sections?, :rule-number 16}
   "detectable_format:json_format" {:function is-json?, :rule-number 17}
   "combination:repeat_prompt" {:function repeat-request?, :rule-number 18}
   "combination:two_responses" {:function two-responses?, :rule-number 19}
   "change_case:english_capital" {:function all-uppercase?, :rule-number 20}
   "change_case:english_lowercase" {:function all-lowercase?, :rule-number 21}
   "change_case:capital_word_frequency" {:function freq-all-capital-words?, :rule-number 22}
   "startend:end_checker" {:function ends-with-response?, :rule-number 23}
   "startend:quotation" {:function wrap-double-quotes?, :rule-number 24}
   "punctuation:no_comma" {:function no-commas?, :rule-number 25}})
