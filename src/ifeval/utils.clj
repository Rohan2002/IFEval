(ns ifeval.utils 
  (:require [cld.core :as lang]
            [clojure.string :as str]
            [dk.simongray.datalinguist :as dk]))

(lang/default-init!)

(defn language-detect?
  "Use https://github.com/dakrone/cld to detect language of text"
  [text]
  (first (lang/detect text)))

(defn replace-punctuation-with-space
  "Remove punctuations from the string"
  [s]
  (str/replace s #"\p{Punct}" ""))

(def function-map
  {"less than" #(< %1 %2)
   "more than" #(> %1 %2)
   "at least" #(>= %1 %2)
   "at most" #(<= %1 %2)
   "exactly" #(= %1 %2)})

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
  (str/split corpus divider))

(defn number-of-keyword-in-corpus
  "Count how many times keyword occurs in corpus"
  [corpus keyword]
  (->>
   (get-words-from-corpus corpus)
   (filter #(= keyword %))
   (count)))

(defn all-lowercase?
  "Check if string s is all lowercase"
  [s]
  (= s (str/lower-case s)))

(defn all-uppercase? 
  "Check if string s is all uppercase"
  [s]
  (= s (str/upper-case s)))

(def dev-null-function (fn [& args]))