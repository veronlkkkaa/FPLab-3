(ns lab3.interpolation)

;; === Вспомогательные функции для управления коллекциями ===

(defn trim-queue
  "Ограничение размера очереди: сохраняет только n последних элементов"
  [queue n]
  (let [size (count queue)]
    (if (> size n)
      (reduce (fn [q _] (pop q)) queue (range (- size n)))
      queue)))

;; Полиморфные операции для алгоритмов интерполяции

(defmulti append-data-point
  "Включение новой входной точки в набор данных алгоритма"
  (fn [algorithm-type _ _] algorithm-type))

(defmulti has-sufficient-data?
  "Определяет, накоплено ли достаточно точек для интерполяции"
  (fn [algorithm-type _ _] algorithm-type))

(defmulti compute-interpolation
  "Вычисление интерполированного значения в заданной точке x"
  (fn [algorithm-type _ _] algorithm-type))

(defmulti reduce-state-size
  "Оптимизация памяти: удаление избыточных данных из состояния"
  (fn [algorithm-type _config _current-state] algorithm-type))

;; Линейная интерполяция


(defn locate-interval
  "Находит два последовательных узла [p1 p2], между которыми лежит target-x"
  [data-points target-x]
  (when (>= (count data-points) 2)
    (reduce (fn [_ [pt1 pt2]]
              (when (and (<= (:x pt1) target-x)
                         (<= target-x (:x pt2)))
                (reduced [pt1 pt2])))
            nil
            (partition 2 1 data-points))))

(defn calculate-linear-interpolation
  "Вычисление y по линейной интерполяции между двумя точками"
  [data-points target-x]
  (when-let [[pt1 pt2] (locate-interval data-points target-x)]
    (let [{x1 :x y1 :y} pt1
          {x2 :x y2 :y} pt2]
      (if (== x1 x2)
        y1
        (let [ratio (/ (- target-x x1) (- x2 x1))]
          (+ y1 (* ratio (- y2 y1))))))))

;; Реализация методов для линейной интерполяции

(defmethod append-data-point :linear [_ state new-point]
  (update state :points conj new-point))

(defmethod has-sufficient-data? :linear [_ _config state]
  (>= (count (:points state)) 2))

(defmethod compute-interpolation :linear [_ state target-x]
  (when-some [interpolated-y (calculate-linear-interpolation (:points state) target-x)]
    {:alg :linear :x target-x :y interpolated-y}))

(defmethod reduce-state-size :linear [_ _config state]
  (update state :points trim-queue 2))

;; Интерполяция методом Ньютона


(defn select-optimal-window
  "Выбор оптимального окна точек для интерполяции Ньютона"
  [data-points window-size target-x]
  (let [total-points (count data-points)]
    (cond
      (zero? total-points)
      []

      (<= total-points window-size)
      (vec data-points)

      :else
      (let [nearest-index
            (->> data-points
                 (map-indexed vector)
                 (map (fn [[idx pt]]
                        [idx (Math/abs ^double (- target-x (:x pt)))]))
                 (apply min-key second)
                 first)

            actual-size (min window-size total-points)
            half-window (quot (dec actual-size) 2)
            raw-start (- nearest-index half-window)
            window-start (-> raw-start
                             (max 0)
                             (min (- total-points actual-size)))]

        (subvec (vec data-points) window-start (+ window-start actual-size))))))

(defn build-difference-table
  "Построение таблицы разделенных разностей для полинома Ньютона"
  [data-points]
  (let [x-coords (mapv :x data-points)
        y-coords (mapv :y data-points)
        num-points (count data-points)]
    (loop [level 0
           current-table y-coords
           coefficients []]
      (if (= level num-points)
        coefficients
        (let [next-coeff (conj coefficients (first current-table))
              next-table
              (if (= level (dec num-points))
                []
                (mapv (fn [idx]
                        (let [numerator (- (current-table (inc idx))
                                           (current-table idx))
                              denominator (- (x-coords (+ idx level 1))
                                             (x-coords idx))]
                          (/ numerator denominator)))
                      (range (- num-points level 1))))]
          (recur (inc level) next-table next-coeff))))))

(defn evaluate-newton-polynomial
  "Вычисление значения полинома Ньютона в точке по схеме Горнера"
  [coefficients data-points target-x]
  (let [x-values (mapv :x data-points)
        degree (count coefficients)]
    (loop [position (dec degree)
           result (nth coefficients (dec degree))]
      (if (zero? position)
        result
        (let [prev-pos (dec position)
              term (* (- target-x (x-values prev-pos)) result)]
          (recur prev-pos (+ (nth coefficients prev-pos) term)))))))

(defn calculate-newton-interpolation
  "Расчёт интерполированного значения методом Ньютона"
  [data-points window-size target-x]
  (let [selected-points (select-optimal-window data-points window-size target-x)
        polynomial-coeffs (build-difference-table selected-points)]
    (evaluate-newton-polynomial polynomial-coeffs selected-points target-x)))

;; Реализация методов для интерполяции Ньютона

(defmethod append-data-point :newton [_ state new-point]
  (update state :points conj new-point))

(defmethod has-sufficient-data? :newton [_ config state]
  (>= (count (:points state)) (:n config)))

(defmethod compute-interpolation :newton [_ state target-x]
  (let [available-points (:points state)
        required-points (:n state)]
    (when (and required-points (>= (count available-points) required-points))
      (let [interpolated-y (calculate-newton-interpolation
                            available-points
                            required-points
                            target-x)]
        {:alg :newton :x target-x :y interpolated-y}))))

(defmethod reduce-state-size :newton [_ _config state]
  (let [required-points (:n state)]
    (if required-points
      (update state :points trim-queue (inc required-points))
      state)))

;; Потоковая обработка данных

(defn sanitize-zero
  "Нормализация отрицательного нуля к положительному"
  [value]
  (let [num (double value)]
    (if (== num -0.0) 0.0 num)))

(defn create-initial-state
  "Создание начального состояния для всех алгоритмов"
  []
  {:linear {:points clojure.lang.PersistentQueue/EMPTY
            :next-x nil}
   :newton {:points clojure.lang.PersistentQueue/EMPTY
            :next-x nil}})

(defn generate-outputs-for-algorithm
  "Генерация выходных точек для одного алгоритма в заданном диапазоне"
  [algorithm-type configuration algorithm-state upper-bound]
  (if-not (has-sufficient-data? algorithm-type configuration algorithm-state)
    {:state algorithm-state :outputs []}

    (let [discretization-step (:step configuration)
          starting-point (or (:next-x algorithm-state)
                             (some-> algorithm-state :points first :x))]
      (if (nil? starting-point)
        {:state algorithm-state :outputs []}

        (loop [current-x starting-point
               accumulated-outputs []]
          (if (> current-x upper-bound)
            {:state (assoc algorithm-state :next-x current-x)
             :outputs accumulated-outputs}

            (let [interpolation-result (compute-interpolation
                                        algorithm-type
                                        algorithm-state
                                        current-x)
                  new-outputs (if interpolation-result
                                (conj accumulated-outputs interpolation-result)
                                accumulated-outputs)]
              (recur (+ current-x discretization-step) new-outputs))))))))

(defn generate-all-outputs
  "Генерация выходных данных для всех активных алгоритмов"
  [configuration state upper-x-bound]
  (let [linear-result
        (if (:linear? configuration)
          (generate-outputs-for-algorithm :linear configuration (:linear state) upper-x-bound)
          {:state (:linear state) :outputs []})

        newton-result
        (if (:newton? configuration)
          (generate-outputs-for-algorithm :newton configuration (:newton state) upper-x-bound)
          {:state (:newton state) :outputs []})

        combined-outputs (into (:outputs linear-result) (:outputs newton-result))]

    {:state {:linear (:state linear-result)
             :newton (:state newton-result)}
     :outputs combined-outputs}))

(defn handle-new-point
  "Основная функция потоковой обработки: принимает точку, возвращает интерполяцию"
  [configuration state incoming-point]
  (let [;; Добавляем точку в состояние каждого активного алгоритма
        updated-linear-state
        (if (:linear? configuration)
          (append-data-point :linear (:linear state) incoming-point)
          (:linear state))

        updated-newton-state
        (if (:newton? configuration)
          (assoc (append-data-point :newton (:newton state) incoming-point)
                 :n (:n configuration))
          (:newton state))

        ;; Ограничиваем размер состояния
        trimmed-linear-state
        (if (:linear? configuration)
          (reduce-state-size :linear configuration updated-linear-state)
          updated-linear-state)

        trimmed-newton-state
        (if (:newton? configuration)
          (reduce-state-size :newton configuration updated-newton-state)
          updated-newton-state)

        current-state {:linear trimmed-linear-state
                       :newton trimmed-newton-state}

        max-x-value (:x incoming-point)

        result (generate-all-outputs configuration current-state max-x-value)]

    {:state (:state result)
     :outputs (:outputs result)}))