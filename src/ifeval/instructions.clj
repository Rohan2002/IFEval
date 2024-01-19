(ns ifeval.instructions
  (:require [clojure.walk :as walk]
            [clojure.string :as string]
            [clojure.set :as s]
            [cld.core :as lang]
            [cheshire.core :as ches]
            [dk.simongray.datalinguist :refer :all :as dk]))

(lang/default-init!)

(defn keywords-to-clojure-key
  [keywords]
  (walk/keywordize-keys keywords))

(defn replace-punctuation-with-space
  "Remove punctuations from the string"
  [s]
  (clojure.string/replace s #"\p{Punct}" ""))

(defn decouple-corpus
  "Get the tokens (words) and sentences from a corpus of text. 
     Using the Stanford Core NLP library to do this task because
     it can handel multiple languages. We don't need language models
     for this task because tokenization and sentence splitting does not require it 
     as mentioned over here https://rb.gy/5i6a8r
     "
  [corpus]
  (->>
   ((dk/->pipeline {:annotators ["tokenize" "ssplit"]}) corpus)
   (dk/recur-datafy)))

(def optimized-decouple-corpus
  "Memoize version of decouple-corpus"
  (memoize decouple-corpus))

(defn get-words-from-corpus
  "Return a vector of words extracted from a corpus"
  [corpus]
  (->>
   (get (optimized-decouple-corpus (replace-punctuation-with-space corpus)) :tokens)
   (map #(get % :original-text))))

(defn get-sentences-from-corpus
  "Return a vector of words extracted from a corpus"
  [corpus]
  (->>
   (get (optimized-decouple-corpus corpus) :sentences)
   (map #(get % :text))))

(defn get-paragraph-from-corpus
  "Return a vector of paragraph extracted from a corpus.
   Paragraphs in the IFEval paper are either divided by
   *** markdown or \"\n\n\" two line breaks.
   "
  [corpus divider]
  (string/split corpus divider))

(defn keywords-contain?
  "Return the keywords that exist in the corpus of text (IFeval rule 1) (Include Keywords)"
  [corpus keywords]
  (s/intersection (set (get-words-from-corpus corpus)) (set (map name keywords))))

(defn keywords-freq?
  "Check if keyword appears n times in corpus (IFeval rule 2) (Keyword Frequency)"
  [corpus keyword n]
  (->
   (frequencies (get-words-from-corpus corpus))
   (get keyword 0)
   (= n)))

(defn keywords-forbidden?
  "Check if at least one keyword from keywords appears in corpus (IFeval rule 3) (Forbidden Words)"
  [corpus forbidden-keywords n]
  (->
   (count (keywords-contain? corpus forbidden-keywords))
   (= n)))

(defn keywords-character-freq?
  "Check if corpus has n number of characters (IFeval rule 4) (Letter Frequency)"
  [corpus character n]
  (->
   (frequencies corpus)
   (get (.charAt character 0) 0)
   (= n)))

(defn language-map
  "Parse language.json into clojure map"
  []
  (walk/keywordize-keys (ches/parse-string (slurp "resources/languages.json"))))

(defn language-detect?
  "Use https://github.com/dakrone/cld to detect language of text (IFEval rule 5) (Response Language)"
  [corpus language_code]
  (->
   (first (lang/detect corpus))
   (= language_code)))

(def function-map
  {"less than" #(< %1 %2)
   "more than" #(> %1 %2)
   "at least" #(>= %1 %2)
   "at most" #(<= %1 %2)
   "exactly" #(= %1 %2)})

(defn num-paragraphs?
  "Check if corpus has exactly n paragraphs. Paragraphs are seperated by markdown divider *** (IFeval rule 6) (Number Paragraphs)"
  [corpus n]
  (->
   (count (get-paragraph-from-corpus corpus #"\*\*\*"))
   (= n)))

(defn num-words?
  "Check if corpus has \"at least\", \"at most\", \"less than\", \"more than\", \"exactly\" n words (IFEval rule 7) (Number Words)"
  [corpus relation n]
  (->
   (count (get-words-from-corpus corpus))
   ((get function-map relation "exactly") n)))

(defn num-sentences?
  "Check if corpus has \"at least\", \"at most\", \"less than\", \"more than\", \"exactly\" n sentences (IFEval rule 8) (Number Sentences)"
  [corpus relation n]
  (->
   (count (get-sentences-from-corpus corpus))
   ((get function-map relation "exactly") n)))

(defn num-paragraphs-with-starting-word?
  "There should be {N} paragraphs. 
   Paragraphs and only paragraphs are separated with each other by two line breaks. 
   The {i}-th paragraph must start with word {first word}.
   (IFEval rule 9) (Number Paragraphs + First Word in i-th Paragraph)"
  [corpus n ith-paragraph-number first-word]
  (let [paragraphs (get-paragraph-from-corpus corpus #"\n\n")
        first-word-function (comp first get-words-from-corpus)]
    (let [first-word-of-paragraphs (map first-word-function paragraphs)
          nth-first-word (nth first-word-of-paragraphs (- ith-paragraph-number 1) 0)]
      (and (= (count first-word-of-paragraphs) n) (= nth-first-word first-word)))))

(defn postscript-detect?
  "At the end of your response, please explicitly add a postscript starting with {postscript marker} (IFEval rule 10) (Postscript)"
  [corpus postscript-marker]
  (->
   (last (get-paragraph-from-corpus corpus #"\n\n"))
   (string/starts-with? postscript-marker)))

(defn number-of-keyword-in-corpus
  "Count how many times keyword occurs in corpus"
  [corpus keyword]
  (->>
   (get-words-from-corpus corpus)
   (filter #(= keyword %))
   (count)))

(defn placeholders-detect?
  "The response must contain at least {N} placeholders represented by square brackets, such as [address]. (IFEval rule 11) (Number Placeholder)"
  [corpus placeholder n]
  (->>
   (number-of-keyword-in-corpus corpus placeholder)
   ((get function-map "at least") n)))

(defn bullets-detect?
  "Your answer must contain exactly {N} bullet points. Use the markdown bullet points such as: * This is a point. (IFEval rule 12) (Number Bullets)"
  [corpus n]
  (->>
   (number-of-keyword-in-corpus corpus "*")
   ((get function-map "exactly") n)))

(defn contain-title?
  "Your answer must contain a title, wrapped in double angular brackets, such as <<poem of joy>>. (IFEval rule 13) (Title)"
  [corpus]
  (->
   (re-find #"[^<]<<([^>>]+)>>[^>]" corpus)
   (count)
   ((get function-map "at least") 1)))

(defn choose-from
  "Answer with one of the following options: {options}. (IFEval rule 14) (Choose From)"
  [corpus options])
(defn highlight-n-sections?
  "Highlight at least {N} sections in your answer with markdown, i.e. *highlighted section* (IFeval rule 15) (Min highlighted sections)"
  [corpus n])

(defn multiple-sections?
  "Your response must have {N} sections. Mark the beginning of each section with {section splitter} X. (IFeval rule 16) (Multiple sections)"
  [corpus n section-splitter])

(defn is-json?
  "Is response in json format? (IFEval rule 17) (JSON Format)"
  [corpus])

(defn repeat-request?
  "First, repeat the request without change, then give your answer (do not say anything before repeating the request; the request you need to repeat does not include this sentence) (IFeval rule 18) (Repeat Prompt)"
  [corpus request])

(defn two-responses?
  "Give two different responses. Responses and only responses should be separated by 6 asterisk symbols: ******. (IFeval rule 19) (Two Responses)"
  [corpus response-seperator])

(defn all-uppercase?
  "Your entire response should be in English, capital letters only. (IFeval rule 20) (All Uppercase)"
  [corpus])

(defn all-lowercase?
  "Your entire response should be in English, and in all lowercase letters. No capital letters are allowed. (IFeval rule 21) (All Lowecase)"
  [corpus])

(defn freq-all-capital-words?
  "In your response, words with all capital letters should appear at least / around / at most {N} times (IFeval rule 22) (Frequency of All Capital Words)"
  [corpus n])

(defn ends-with-response?
  "Finish your response with this exact phrase {end phrase}. No other words should follow this phrase. (IFeval rule 23) (End Checker)"
  [corpus end-phrase])

(defn wrap-double-quotes?
  "Wrap your entire response with double quotation marks. (IFeval rule 24) (Quotation)"
  [corpus end-phrase])

(defn no-commas?
  "In your entire response, refrain from the use of any commas. (IFeval rule 25) (No Commas)"
  [corpus])

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
