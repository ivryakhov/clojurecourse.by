(ns dsl.core
  (:use clojure.walk)
  (:require [clj-time.core :as t]
            [clojure.walk :as w]))

(def today (t/now))
(def yesterday (t/minus today (t/days 1)))
(def tomorrow (t/plus today (t/days 1)))

(comment
  (defn one [] 1)

  ;; Примеры вызова
  (with-datetime
    (if (> today tomorrow) (println "Time goes wrong"))
    (if (<= yesterday today) (println "Correct"))
    (let [six (+ 1 2 3)
          d1 (today - 2 days)
          d2 (today + 1 week)
          d3 (today + six months)
          d4 (today + (one) year)]
      (if (and (< d1 d2)
               (< d2 d3)
               (< d3 d4))
        (println "DSL works correctly")))))


(defn d-op [op d1 d2]
  (if (and (instance? org.joda.time.DateTime d1)
           (instance? org.joda.time.DateTime d2))
    (cond 
     (= op >) (t/after? d1 d2)
     (= op <) (t/before? d1 d2)
     (= op >=) (or (t/after? d1 d2)
                   (= d1 d2))
     (= op <=) (or (t/before? d1 d2)
                   (= d1 d2))
     :else (op d1 d2))
    (op d1 d2)))


(defmacro date-operate [date op num period]
  (let [t-op (case op
               + t/plus
               - t/minus)
        t-period (case period
                        (second seconds) t/seconds
                        (minute minutes) t/minutes
                        (hour hours) t/hours
                        (day days) t/days
                        (week weeks) t/weeks
                        (month months) t/months
                        (year years) t/years)]
    `(~t-op ~date (~t-period ~num))))

;; Можете использовать эту функцию для того, чтобы определить,
;; является ли список из 4-х элементов тем самым списком, который
;; создает новую дату,
;; и который нужно обработать функцией d-add.
(defn is-date-operate? [nlist]
  (let [op (second nlist)
        period (last nlist)]
    (and (= (count nlist) 4)
         (or (= '+ op)
             (= '- op))
         (contains? #{'day 'days 'week 'weeks 'month 'months 'year 'years
                      'hour 'hours 'minute 'minutes 'second 'seconds} period))))

(defn is-dates-compare? [nlist]
  (if (= (count nlist) 3)
    (let [op (str (first nlist))]
      (contains? #{">" "<" ">=" "<="} op))))


(defn if-replace [code]
  (if (list? code)
    (cond
     (is-dates-compare? code) (conj code d-op)
     (is-date-operate? code) `(date-operate ~@code)
     :else code)
    code))

(defmacro with-datetime [& code]
  `~(conj (w/postwalk #(if-replace %) code) 'do))


