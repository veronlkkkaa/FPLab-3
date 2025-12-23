(ns lab3.core
  (:require [clojure.string :as string]
            [lab3.interpolation :as interpolator])
  (:gen-class))

;; Вспомогательные функции для парсинга

(defn str->double [string-value]
  (Double/parseDouble string-value))

(defn str->int [string-value]
  (Integer/parseInt string-value))

;; === Парсинг командной строки ===

(defn validate-configuration
  "Валидация параметров: проверяет наличие алгоритма и корректность шага"
  [config]
  (when-not (or (:linear? config) (:newton? config))
    (binding [*out* *err*]
      (println "Error: at least one of --linear or --newton must be specified"))
    (System/exit 1))

  (when (<= (:step config) 0)
    (binding [*out* *err*]
      (println "Error: --step must be positive"))
    (System/exit 1))

  config)

(defn process-arguments
  "Обработка аргументов командной строки и создание конфигурации"
  [arguments]
  (let [default-config {:linear? false
                        :newton? false
                        :step 1.0
                        :n 4}]
    (loop [config default-config
           remaining-args arguments]

      (if (empty? remaining-args)
        (validate-configuration config)

        (let [[current-arg & other-args] remaining-args]
          (cond
            (= current-arg "--linear")
            (recur (assoc config :linear? true) other-args)

            (= current-arg "--newton")
            (recur (assoc config :newton? true) other-args)

            (= current-arg "--step")
            (if-let [step-value (first other-args)]
              (recur (assoc config :step (str->double step-value)) (rest other-args))
              (throw (ex-info "--step requires a value" {})))

            (or (= current-arg "-n") (= current-arg "--n"))
            (if-let [n-value (first other-args)]
              (recur (assoc config :n (str->int n-value)) (rest other-args))
              (throw (ex-info "-n/--n requires a value" {})))

            :else
            (throw (ex-info (str "Unknown argument: " current-arg) {}))))))))

;; Парсинг входных данных

(defn extract-point-from-line
  "Парсит строку формата 'x y' или 'x;y' или 'x,y' в структуру {:x ... :y ...}"
  [input-line]
  (let [trimmed-line (string/trim input-line)]
    (when-not (string/blank? trimmed-line)
      (let [components (string/split trimmed-line #"[;,\s\t]+")]
        (cond
          (< (count components) 2)
          (do
            (binding [*out* *err*]
              (println "Warning: insufficient data in line:" trimmed-line))
            nil)

          :else
          (try
            {:x (str->double (first components))
             :y (str->double (second components))}
            (catch NumberFormatException e
              (binding [*out* *err*]
                (println "Warning: invalid number format in line:" trimmed-line))
              nil)))))))

;; === Форматирование выходных данных ===

(defn format-number
  "Форматирование числа с удалением незначащих нулей"
  [number]
  (let [rounded-value (/ (Math/round (* number 1000.0)) 1000.0)
        formatted-string (String/format
                          java.util.Locale/US
                          "%.3f"
                          (to-array [(double rounded-value)]))]
    (.replaceFirst formatted-string "\\.?0+$" "")))

(defn print-result
  "Вывод результата интерполяции"
  [{:keys [alg x y]}]
  (println (str (name alg) ": " (format-number x) " " (format-number y))))

;; === Основная логика программы ===

(defn process-input-stream
  "Читает stdin построчно, обрабатывает каждую точку, выводит результаты"
  [config initial-state]
  (loop [current-state initial-state
         last-x nil]
    (if-some [input-line (read-line)]
      ;; Есть входные данные - обрабатываем
      (if-let [data-point (extract-point-from-line input-line)]
        (let [result (interpolator/handle-new-point config current-state data-point)]
          (doseq [output (:outputs result)]
            (print-result output))
          (recur (:state result) (:x data-point)))
        ;; Некорректная строка - игнорируем
        (recur current-state last-x))

      ;; EOF достигнут - возвращаем финальное состояние и последний x
      {:final-state current-state
       :last-x last-x})))

(defn finalize-interpolation
  "Вывод оставшихся точек после EOF"
  [config state last-x]
  (when last-x
    (let [step (:step config)
          ;; Определяем максимальную границу для финальной интерполяции
          max-x (+ last-x (* step 3))]
      (when (:linear? config)
        (let [linear-state (:linear state)
              next-x (or (:next-x linear-state) last-x)]
          (when (interpolator/has-sufficient-data? :linear config linear-state)
            (loop [x next-x]
              (when (<= x max-x)
                (when-let [result (interpolator/compute-interpolation :linear linear-state x)]
                  (print-result result))
                (recur (+ x step)))))))

      (when (:newton? config)
        (let [newton-state (:newton state)
              next-x (or (:next-x newton-state) last-x)]
          (when (interpolator/has-sufficient-data? :newton config newton-state)
            (loop [x next-x]
              (when (<= x max-x)
                (when-let [result (interpolator/compute-interpolation :newton newton-state x)]
                  (print-result result))
                (recur (+ x step))))))))))

(defn -main [& args]
  (let [configuration (process-arguments args)
        initial-state (interpolator/create-initial-state)
        {:keys [final-state last-x]} (process-input-stream configuration initial-state)]

    ;; После EOF выводим оставшиеся точки
    (finalize-interpolation configuration final-state last-x)))