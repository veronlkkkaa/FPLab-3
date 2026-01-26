(ns lab3.interpolation)

(defn limit-queue
  "Обрезает очередь до max-size, выкидывая элементы слева."
  [q max-size]
  (loop [q q]
    (if (<= (count q) max-size)
      q
      (recur (pop q)))))

;; Полиморфизм

(defmulti process-interpolation
  "Главный полиморфный интерфейс для интерполяции.
   Диспетчеризация: по алгоритму

   Принимает state с полями:
   - :algorithm  - алгоритм (:linear или :newton)
   - :step       - шаг интерполяции
   - :points     - очередь точек
   - :new-point  - новая добавляемая точка
   - :next-x     - следующий x для вывода
   - :n          - параметр n (для Newton)
   - :max-x      - максимальный x (x новой точки)

   Возвращает {:state новое_состояние :outputs [{:alg :x :y}...]}"
  :algorithm)

(defmulti process-interpolator
  "Обрабатывает отдельный интерполятор.
   Диспетчеризация: по типу интерполятора (:type)

   Принимает интерполятор с полями:
   - :type       - тип (:linear или :newton)
   - :points     - очередь точек
   - :next-x     - следующий x для вывода
   - :n          - параметр n (для Newton, опционально)

   И параметры:
   - new-point   - новая точка
   - step        - шаг интерполяции
   - max-x       - максимальный x

   Возвращает обновленный интерполятор с выходными данными:
   {:interpolator новый_интерполятор :outputs [{:alg :x :y}...]}"
  (fn [interpolator _ _ _] (:type interpolator)))

;; Linear

(defn find-segment
  "Находит пару соседних точек [p1 p2], таких что x1 <= x <= x2."
  [points x]
  (when (>= (count points) 2)
    (some (fn [[p1 p2]]
            (when (and (<= (:x p1) x)
                       (<= x (:x p2)))
              [p1 p2]))
          (partition 2 1 points))))

(defn linear-interpolate
  "Вычисляет линейную интерполяцию в точке x"
  [points x]
  (when-let [[p1 p2] (find-segment points x)]
    (let [x1 (:x p1)
          y1 (:y p1)
          x2 (:x p2)
          y2 (:y p2)]
      (if (= x1 x2)
        y1
        (let [t (/ (- x x1) (- x2 x1))]
          (+ y1 (* t (- y2 y1))))))))

(defmethod process-interpolation :linear
  [{:keys [points new-point step next-x max-x] :as state}]
  (let [;; Добавляем новую точку и обрезаем очередь до 2 элементов
        new-points (-> points
                       (conj new-point)
                       (limit-queue 2))

        ;; Проверяем готовность (нужно >= 2 точек)
        ready? (>= (count new-points) 2)]

    (if-not ready?
      {:state (assoc state :points new-points :next-x nil)
       :outputs []}

      (let [;; Определяем начальный x: если нет next-x, начинаем с первой точки
            start-x (or next-x (:x (first new-points)))

            ;; Генерируем выходы
            outputs (loop [x start-x
                           outs []]
                      (if (> x max-x)
                        outs
                        (if-let [y (linear-interpolate new-points x)]
                          (recur (+ x step)
                                 (conj outs {:alg :linear :x x :y y}))
                          (recur (+ x step) outs))))

            ;; Вычисляем новый next-x
            new-next-x (if (empty? outputs)
                         start-x  ; Если ничего не вывели, сохраняем start-x
                         (let [last-out-x (:x (last outputs))]
                           (+ last-out-x step)))]

        {:state (assoc state
                       :points new-points
                       :next-x new-next-x)
         :outputs outputs}))))

(defmethod process-interpolator :linear
  [interpolator new-point step max-x]
  (let [points (:points interpolator)
        next-x (:next-x interpolator)
        ;; Добавляем новую точку и обрезаем очередь до 2 элементов
        new-points (-> points
                       (conj new-point)
                       (limit-queue 2))

        ;; Проверяем готовность (нужно >= 2 точек)
        ready? (>= (count new-points) 2)]

    (if-not ready?
      {:interpolator (assoc interpolator :points new-points :next-x nil)
       :outputs []}

      (let [;; Определяем начальный x: если нет next-x, начинаем с первой точки
            start-x (or next-x (:x (first new-points)))

            ;; Генерируем выходы
            outputs (loop [x start-x
                           outs []]
                      (if (> x max-x)
                        outs
                        (if-let [y (linear-interpolate new-points x)]
                          (recur (+ x step)
                                 (conj outs {:alg :linear :x x :y y}))
                          (recur (+ x step) outs))))

            ;; Вычисляем новый next-x
            new-next-x (if (empty? outputs)
                         start-x  ; Если ничего не вывели, сохраняем start-x
                         (let [last-out-x (:x (last outputs))]
                           (+ last-out-x step)))]

        {:interpolator (assoc interpolator
                              :points new-points
                              :next-x new-next-x)
         :outputs outputs}))))

;; Newton

(defn choose-window
  "Выбирает окно из n точек для интерполяции Ньютона"
  [points n x]
  (let [cnt (count points)]
    (cond
      (zero? cnt) []
      (<= cnt n)  (vec points)

      :else
      (let [closest-idx
            (->> points
                 (map-indexed (fn [i p]
                                [i (Math/abs ^double (- x (:x p)))]))
                 (apply min-key second)
                 first)

            n' (min n cnt)
            half (quot (dec n') 2)
            raw-start (- closest-idx half)
            start (-> raw-start
                      (max 0)
                      (min (- cnt n')))]
        (subvec (vec points) start (+ start n'))))))

(defn calc-coefficients
  "Строит таблицу разделённых разностей.
   Возвращает вектор коэффициентов [a0 a1 a2 ...]"
  [points]
  (let [xs (mapv :x points)
        ys (mapv :y points)
        n  (count points)]
    (loop [k 0 table ys acc []]
      (if (= k n)
        acc
        (let [acc' (conj acc (first table))
              table'
              (if (= k (dec n))
                []
                (mapv (fn [i]
                        (/ (- (table (inc i))
                              (table i))
                           (- (xs (+ i k 1))
                              (xs i))))
                      (range 0 (- n k 1))))]
          (recur (inc k) table' acc'))))))

(defn newton-eval
  "Вычисляет значение полинома Ньютона в точке x"
  [coeffs points x]
  (let [xs (mapv :x points)
        n  (count coeffs)]
    (loop [k (dec n)
           acc (nth coeffs (dec n))]
      (if (zero? k)
        acc
        (let [k' (dec k)]
          (recur k'
                 (+ (nth coeffs k')
                    (* (- x (xs k')) acc))))))))

(defn newton-interpolate
  "Вычисляет интерполяцию Ньютона в точке x"
  [points n x]
  (let [window (choose-window points n x)
        coeffs (calc-coefficients window)]
    (newton-eval coeffs window x)))

(defmethod process-interpolation :newton
  [{:keys [points new-point step next-x max-x n] :as state}]
  (let [;; Добавляем новую точку и обрезаем очередь до (n+1) элементов
        max-points (inc n)
        new-points (-> points
                       (conj new-point)
                       (limit-queue max-points))

        ;; Проверяем готовность (нужно >= n точек)
        ready? (>= (count new-points) n)]

    (if-not ready?
      {:state (assoc state :points new-points :next-x nil)
       :outputs []}

      (let [;; Определяем начальный x: если нет next-x, начинаем с первой точки
            start-x (or next-x (:x (first new-points)))

            ;; Генерируем выходы
            outputs (loop [x start-x
                           outs []]
                      (if (> x max-x)
                        outs
                        (let [y (newton-interpolate new-points n x)]
                          (recur (+ x step)
                                 (conj outs {:alg :newton :x x :y y})))))

            ;; Вычисляем новый next-x
            new-next-x (if (empty? outputs)
                         start-x  ; Если ничего не вывели, сохраняем start-x
                         (let [last-out-x (:x (last outputs))]
                           (+ last-out-x step)))]

        {:state (assoc state
                       :points new-points
                       :next-x new-next-x)
         :outputs outputs}))))

(defmethod process-interpolator :newton
  [interpolator new-point step max-x]
  (let [points (:points interpolator)
        next-x (:next-x interpolator)
        n (:n interpolator)
        ;; Добавляем новую точку и обрезаем очередь до (n+1) элементов
        max-points (inc n)
        new-points (-> points
                       (conj new-point)
                       (limit-queue max-points))

        ;; Проверяем готовность (нужно >= n точек)
        ready? (>= (count new-points) n)]

    (if-not ready?
      {:interpolator (assoc interpolator :points new-points :next-x nil)
       :outputs []}

      (let [;; Определяем начальный x: если нет next-x, начинаем с первой точки
            start-x (or next-x (:x (first new-points)))

            ;; Генерируем выходы
            outputs (loop [x start-x
                           outs []]
                      (if (> x max-x)
                        outs
                        (let [y (newton-interpolate new-points n x)]
                          (recur (+ x step)
                                 (conj outs {:alg :newton :x x :y y})))))

            ;; Вычисляем новый next-x
            new-next-x (if (empty? outputs)
                         start-x  ; Если ничего не вывели, сохраняем start-x
                         (let [last-out-x (:x (last outputs))]
                           (+ last-out-x step)))]

        {:interpolator (assoc interpolator
                              :points new-points
                              :next-x new-next-x)
         :outputs outputs}))))

;; State management

(defn normalize-zero [x]
  (let [d (double x)]
    (if (= d -0.0) 0.0 d)))

(defn init-state [opts]
  "Инициализирует вектор интерполяторов на основе опций"
  (cond-> []
    (:linear? opts)
    (conj {:type :linear
           :points clojure.lang.PersistentQueue/EMPTY
           :next-x nil})

    (:newton? opts)
    (conj {:type :newton
           :points clojure.lang.PersistentQueue/EMPTY
           :next-x nil
           :n (:n opts)})))

(defn handle-datapoint
  "Обрабатывает входящую точку для всех активных алгоритмов"
  [opts state point]
  (let [step (:step opts)
        max-x (:x point)

        ;; Обрабатываем все интерполяторы через map
        results (map (fn [interpolator]
                       (process-interpolator interpolator point step max-x))
                     state)

        ;; Извлекаем новые интерполяторы и выходы
        new-interpolators (mapv :interpolator results)
        all-outputs (mapcat :outputs results)]

    {:state new-interpolators
     :outputs (vec all-outputs)}))

(defmulti finalize-interpolator
  "Генерирует оставшиеся выходы для интерполятора после EOF"
  (fn [interpolator _step] (:type interpolator)))

(defmethod finalize-interpolator :linear
  [interpolator step]
  (let [points (:points interpolator)
        next-x (:next-x interpolator)]
    (if (and (>= (count points) 2) next-x)
      (let [max-x (:x (peek points))]
        (loop [x next-x
               outs []]
          (if (> x max-x)
            outs
            (if-let [y (linear-interpolate points x)]
              (recur (+ x step)
                     (conj outs {:alg :linear :x x :y y}))
              (recur (+ x step) outs)))))
      [])))

(defmethod finalize-interpolator :newton
  [interpolator step]
  (let [points (:points interpolator)
        next-x (:next-x interpolator)
        n (:n interpolator)]
    (if (and (>= (count points) n) next-x)
      (let [max-x (:x (peek points))]
        (loop [x next-x
               outs []]
          (if (> x max-x)
            outs
            (let [y (newton-interpolate points n x)]
              (recur (+ x step)
                     (conj outs {:alg :newton :x x :y y}))))))
      [])))

(defn finalize-outputs
  "Генерирует оставшиеся выходы после EOF"
  [opts state]
  (let [step (:step opts)
        all-outputs (mapcat #(finalize-interpolator % step) state)]
    (vec all-outputs)))
