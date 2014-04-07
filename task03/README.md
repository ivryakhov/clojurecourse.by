# Task03

Разработать предметно-ориентированный язык программирования для удобной работы с датами и временем. Основа языка -- макрос with-datetime, который анализирует код внутри себя и выполняет необходимые замены. Внутри этого макроса должны работать операции сравнения двух дат как чисел, а также операции создания новой даты на основе указанной (например: ((java.util.Date.) + 1 day)).

Формат синтаксиса сравнения дат: (> date1 date2), поддерживаемые операторы сравнения -- >, <, >=, <= .

Формат синтаксиса создания даты: (дата операция количество период), где:
* дата -- переменная или вызов функци, возвращающий дату, объект java.util.Date;
* операция -- только + или - ;
* количество -- число, переменная или функция, возвращающая целое число;
* период -- символ периода, поддерживаются следующие периоды: day days week weeks month months year years hour hours minute minutes second seconds. Период в единственном числе (например, day) ничем не отличается от периода во множественном числе (например, days) и служит только для того, чтобы конструкции (today + 1 day) и (today + 2 days) читались "правильно" по-английской грамматике.

Пример использования языка:

;; Заготовки дат
;;
(def cal (java.util.Calendar/getInstance))
(def today (java.util.Date.))
(def yesterday (do (.add cal java.util.Calendar/DATE -1) (.getTime cal)))
(def tomorrow (do (.add cal java.util.Calendar/DATE 2) (.getTime cal)))

;; Использование нашего макроса
;;
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
        (println "DSL works correctly"))))