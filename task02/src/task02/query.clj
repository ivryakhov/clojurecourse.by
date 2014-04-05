(ns task02.query
  (:require [task02.helpers :as helpers]
            [task02.db :as db]
            [clojure.core.match :refer [match]]
            [clojure.string :as string]
            ))

;; Функция выполняющая парсинг запроса переданного пользователем
;;
;; Синтаксис запроса:
;; SELECT table_name [WHERE column comp-op value] [ORDER BY column] [LIMIT N] [JOIN other_table ON left_column = right_column]
;;
;; - Имена колонок указываются в запросе как обычные имена, а не в виде keywords. В
;;   результате необходимо преобразовать в keywords
;; - Поддерживаемые операторы WHERE: =, !=, <, >, <=, >=
;; - Имя таблицы в JOIN указывается в виде строки - оно будет передано функции get-table для получения объекта
;; - Значение value может быть либо числом, либо строкой в одинарных кавычках ('test').
;;   Необходимо вернуть данные соответствующего типа, удалив одинарные кавычки для строки.
;;
;; - Ключевые слова --> case-insensitive
;;
;; Функция должна вернуть последовательность со следующей структурой:
;;  - имя таблицы в виде строки
;;  - остальные параметры которые будут переданы select
;;
;; Если запрос нельзя распарсить, то вернуть nil

;; Примеры вызова:
;; > (parse-select "select student")
;; ("student")
;; > (parse-select "select student where id = 10")
;; ("student" :where #<function>)
;; > (parse-select "select student where id = 10 limit 2")
;; ("student" :where #<function> :limit 2)
;; > (parse-select "select student where id = 10 order by id limit 2")
;; ("student" :where #<function> :order-by :id :limit 2)
;; > (parse-select "select student where id = 10 order by id limit 2 join subject on id = sid")
;; ("student" :where #<function> :order-by :id :limit 2 :joins [[:id "subject" :sid]])
;; > (parse-select "werfwefw")
;; nil

(def sign->fn
  {">" >
   "<" <
   ">=" >=
   "<=" <=
   "=" =
   "!=" not=})

(defn make-where-function
  "Generates function for :where key in query-parsed map"
  [column comp-op value]
  (let [func       (sign->fn comp-op)
        norm-value (if (nil? (re-find #"^\'\S+\'$" value))
                     (helpers/parse-int value)
                     (re-find #"[^']+" value))
        key-column (keyword column)]
    #(func (key-column %) norm-value))
  )  

(defn parse-vector
  "Parses an input-vector of string with matching rules and combines results in result-list"
  ([input-vector result-list]
     (let [[result rest]
           (match input-vector
                  [(:or "select" "SELECT") table-name & rest]
                  [[table-name] rest]
                  
                  [(:or "where" "WHERE") column comp-op value & rest]
                  [[:where (make-where-function column comp-op value)] rest]
                  
                  [(:or "limit" "LIMIT") value & rest]
                  [[:limit (helpers/parse-int value)] rest]
                  
                  [(:or "order" "ORDER") (:or "by" "BY") column & rest]
                  [[:order-by (keyword column)] rest]
                  
                  [(:or "join" "JOIN") other-table (:or "on" "ON") left-column "=" right-column & rest]
                  [[:joins [[(keyword left-column) other-table (keyword right-column)]]] rest]
                  
                  :else
                  [nil nil])]
       
       (if (nil? rest)
         (if (empty? result-list)
           nil
           result-list)
         (recur rest (concat result-list result))))))

(defn parse-select [^String sel-string]
  (let [vec-str (string/split sel-string #"\s")]
    (parse-vector vec-str [])))


;; Выполняет запрос переданный в строке.  Бросает исключение если не удалось распарсить запрос

;; Примеры вызова:
;; > (perform-query "select student")
;; ({:id 1, :year 1998, :surname "Ivanov"} {:id 2, :year 1997, :surname "Petrov"} {:id 3, :year 1996, :surname "Sidorov"})
;; > (perform-query "select student order by year")
;; ({:id 3, :year 1996, :surname "Sidorov"} {:id 2, :year 1997, :surname "Petrov"} {:id 1, :year 1998, :surname "Ivanov"})
;; > (perform-query "select student where id > 1")
;; ({:id 2, :year 1997, :surname "Petrov"} {:id 3, :year 1996, :surname "Sidorov"})
;; > (perform-query "not valid")
;; exception...
(defn perform-query [^String sel-string]
  (if-let [query (parse-select sel-string)]
    (apply db/select (db/get-table (first query)) (rest query))
    (throw (IllegalArgumentException. (str "Can't parse query: " sel-string)))))
