(ns lab3.core
  (:require [clojure.string :as str]
            [lab3.interpolation :as interp])
  (:gen-class))

(defn parse-dbl [s] (Double/parseDouble s))
(defn parse-int [s] (Integer/parseInt s))

(defn parse-args
  "Парсер аргументов командной строки"
  [args]
  (loop [m    {:linear? false
               :newton? false
               :step    1.0
               :n       4}
         args args]
    (if (empty? args)
      (do
        (when (and (not (:linear? m))
                   (not (:newton? m)))
          (binding [*out* *err*]
            (println "Error: at least one of --linear or --newton must be given"))
          (System/exit 1))
        (when (<= (:step m) 0)
          (binding [*out* *err*]
            (println "Error: --step must be > 0"))
          (System/exit 1))
        m)

      (let [[a & rest-args] args]
        (cond
          (= a "--linear")
          (recur (assoc m :linear? true) rest-args)

          (= a "--newton")
          (recur (assoc m :newton? true) rest-args)

          (= a "--step")
          (recur (assoc m :step (parse-dbl (first rest-args)))
                 (rest rest-args))

          (or (= a "-n") (= a "--n"))
          (recur (assoc m :n (parse-int (first rest-args)))
                 (rest rest-args))

          :else
          (throw (ex-info (str "Unexpected argument: " a) {})))))))

(defn parse-point
  "Парсер входных данных"
  [line]
  (let [t (str/trim line)]
    (when-not (str/blank? t)
      (let [parts (str/split t #"[;,\s\t]+")]
        (when (>= (count parts) 2)
          (try
            {:x (parse-dbl (first parts))
             :y (parse-dbl (second parts))}
            (catch NumberFormatException _ nil)))))))

(defn fmt3 [x]
  (let [rounded (/ (Math/round (* x 1000.0)) 1000.0)
        s (String/format java.util.Locale/US "%.3f"
                         (to-array [(double rounded)]))]
    (.replaceFirst s "\\.?0+$" "")))

(defn -main [& args]
  (let [opts (parse-args args)]
    (loop [state (interp/init-state)]
      (if-some [line (read-line)]
        (if-let [p (parse-point line)]
          (let [{:keys [state outputs]}
                (interp/handle-datapoint opts state p)]
            (doseq [{:keys [alg x y]} outputs]
              (println (format "%s: %s %s"
                               (name alg) (fmt3 x) (fmt3 y))))
            (recur state))
          (recur state))
        ;; EOF - финализируем выходы
        (let [outputs (interp/finalize-outputs opts state)]
          (doseq [{:keys [alg x y]} outputs]
            (println (format "%s: %s %s"
                             (name alg) (fmt3 x) (fmt3 y)))))))))