(ns ifeval.core
  (:require
   [clojure.string :as string]
   [cheshire.core :as ches]
   [clojure.walk :as walk]))

(defn read-file
  "Read all lines in file to a vector"
  [file-path]
  (-> (slurp file-path)
      (string/split-lines)))

(defn process-line
  "Parse a line (type string) inside jsonl file to a Clojure map."
  [line]
  (->
   (ches/parse-string line)
   (walk/keywordize-keys)))

(defn process-lines
  "Get all lines in file and process them and return a lazy sequence."
  [file-path]
  (map process-line (read-file file-path)))

(defn construct-data
  "Combine prompt and instructions fields from input_data and response field from prompt_response.jsonl files respectively."
  []
  (let
   [prompt-instructions (process-lines "resources/prompt_instructions.jsonl")
    prompt-response (process-lines "resources/prompt_response.jsonl")]
    (loop [prompt-instructions-looper prompt-instructions
           prompt-response-looper prompt-response
           combined []]
      (if (and (empty? prompt-instructions-looper) (empty? prompt-response-looper))
        combined
        (let [[prompt-instructions & rest-prompt-instructions] prompt-instructions-looper
              [prompt-response & rest-prompt-response] prompt-response-looper]
          (recur rest-prompt-instructions rest-prompt-response (conj combined (merge prompt-instructions (dissoc prompt-response :prompt)))))))))

(defn unique-instructions-id-list
  "Get the unique instruction-id(s) from the input data created from construct-data function."
  [data]
  (->>
   (map #(get % :instruction_id_list) data)
   (flatten)
   (set)
   (sort)
   (reverse)
   ))

(defn -main
  "IFEval main function"
  [& args]
  (construct-data))