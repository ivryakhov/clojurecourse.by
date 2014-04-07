(ns dsl.core-test
  (:require [clojure.test :refer :all]
            [dsl.core :refer :all]))

(deftest comparisons
  (testing "> < >= <="
    (is (with-datetime
          (> today yesterday)))
    (is (with-datetime
          (>= today yesterday)))
    (is (with-datetime
          (< today tomorrow)))
    (is (with-datetime
          (<= today tomorrow)))
    (is (with-datetime
          (> 2 1)))))

(deftest additions
  (let [cal (java.util.Calendar/getInstance)
        today (java.util.Date.)
        yesterday (do (.add cal java.util.Calendar/DATE -1) (.getTime cal))
        tomorrow (do (.add cal java.util.Calendar/DATE 2) (.getTime cal))
        one (fn [] 1)]
    (testing "+ -"
      (is (> (.getTime today)
             (.getTime (with-datetime
                         (and 1 2 3)
                         (today - 2 days)))))
      (is (< (.getTime today)
             (.getTime (with-datetime
                         (today + 1 month)))))
      (is (with-datetime
            (if (> today tomorrow) (println "Time goes wrong"))
            (if (<= yesterday today) (println "Correct"))
            (let [six (+ 1 2 3)
                  d1 (today - 2 days)
                  d2 (today + 1 week)
                  d3 (today + six months)
                  d4 (today + (one) year)]
              (and (< d1 d2)
                   (< d2 d3)
                   (< d3 d4))))))))
