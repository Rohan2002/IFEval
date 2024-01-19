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

