(ns ifeval.core
  (:require
   [clojure.string :as string]
   [cheshire.core :as ches]
   [clojure.walk :as walk]
   [ifeval.utils :as utils]
   [ifeval.instructions :as ins]))

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

(defn loose-comparsion
  "Implementation of the loose IFEval metric."
  [boolean-vector]
  (not (not-any? true? boolean-vector)))

(defn strict-comparsion
  "Implementation of the strict IFEval metric."
  [boolean-vector]
  (not-any? false? boolean-vector))

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
          (recur rest-prompt-instructions rest-prompt-response (conj combined (dissoc (->>
                                                                                       (map vector (get prompt-instructions :instruction_id_list) (get prompt-instructions :kwargs))
                                                                                       (assoc prompt-instructions :response (get prompt-response :response) :function-arg)) :kwargs :instruction_id_list))))))))
(defn apply-instruction-function
  "Instruction function vector and return True if the corpus followed the instruction, or false otherwise."
  [corpus instruction]
  (let [instruction-function (->
                              (get ins/mp-ins-func (get instruction 0) {:function (utils/dev-null-function)})
                              (get :function))
        args (get instruction 1)]
    (if (empty? args) (instruction-function corpus)
        (instruction-function corpus args))))

(defn apply-instruction-function-to-entry
  "Take a single element from construct-data vector and check if response followed the instructions given in the request."
  [comparison data]
  (let [corpus (get data :prompt)
        instructions (get data :function-arg)
        apply-instructions (map (partial apply-instruction-function corpus) instructions)]
    (if (= comparison "loose")
      (loose-comparsion apply-instructions)
      (strict-comparsion apply-instructions))))

(defn apply-instruction-function-to-entries
  "Take a multiple element from construct-data vector and check if response followed the instructions given in the request."
  [data comparison]
  (map (partial apply-instruction-function-to-entry comparison) data))

(defn -main
  "IFEval main function"
  [& args]
  (let
   [data (construct-data)
    loose-data  (apply-instruction-function-to-entries data "loose")
    strict-data (apply-instruction-function-to-entries data "strict")]
    (as->
     (float (/ (count (filter identity loose-data)) (count loose-data))) percentage
      (* 100 percentage)
      (println (str "Prompt-level accuracy for loose IFEval metric is " percentage "%.")))

    (as->
     (float (/ (count (filter identity strict-data)) (count strict-data))) percentage
      (* 100 percentage)
      (println (str "Prompt-level accuracy for strict IFEval metric is " percentage "%.")))))