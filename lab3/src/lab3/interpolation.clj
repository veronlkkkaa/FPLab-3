(ns lab3.interpolation)

(defn limit-queue
  "Обрезает очередь до max-size, выкидывая элементы слева."
  [q max-size]
  (loop [q q]
    (if (<= (count q) max-size)
      q
      (recur (pop q)))))

;; Polymorphism

(defmulti add-point
  "Добавляет точку в состояние алгоритма"
  (fn [alg _ _] alg))

(defmulti alg-ready?
  "Проверяет, готов ли алгоритм к интерполяции (достаточно ли точек и параметров)."
  (fn [alg _ _] alg))

(defmulti interpolate
  "Вычисляет значение интерполяции алгоритма в точке x.
   Возвращает либо {:alg :linear/:newton :x x :y y}, либо nil."
  (fn [alg _ _] alg))

(defmulti limit-state
  "Ограничивает размер state."
  (fn [alg _opts _state] alg))

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

(defn compute-linear
  "Линейная интерполяция: возвращает y или nil."
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

;; Реализация мульти-методов для линейной интерполяции

(defmethod add-point :linear [_ state point]
  (update state :points conj point))

(defmethod alg-ready? :linear [_ _opts state]
  (>= (count (:points state)) 2))

(defmethod interpolate :linear [_ state x]
  (when-some [y (compute-linear (:points state) x)]
    {:alg :linear :x x :y y}))

(defmethod limit-state :linear [_ _opts state]
  (update state :points limit-queue 2))

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

(defn compute-newton
  "Расчёт y методом Ньютона"
  [points n x]
  (let [window (choose-window points n x)
        coeffs (calc-coefficients window)]
    (newton-eval coeffs window x)))

;; Реализация мульти-методов для Ньютона

(defmethod add-point :newton [_ state point]
  (update state :points conj point))

(defmethod alg-ready? :newton [_ opts state]
  (>= (count (:points state)) (:n opts)))

(defmethod interpolate :newton [_ state x]
  (let [points (:points state)
        n      (:n state)]
    (when (and n (>= (count points) n))
      (let [y (compute-newton points n x)]
        {:alg :newton :x x :y y}))))

(defmethod limit-state :newton [_ _opts state]
  (let [n (:n state)]
    (if n
      (update state :points limit-queue (inc n))
      state)))

  ;; Stream processing

(defn normalize-zero [x]
  (let [d (double x)]
    (if (= d -0.0) 0.0 d)))

(defn init-state []
  {:linear {:points clojure.lang.PersistentQueue/EMPTY
            :next-x nil}
   :newton {:points clojure.lang.PersistentQueue/EMPTY
            :next-x nil}})

(defn interpolate-at-x
  "Возвращает вектор структур {:alg :linear :x x :y y}, ..."
  [opts state x]
  (let [res1 (when (:linear? opts)
               (interpolate :linear (:linear state) x))
        res2 (when (:newton? opts)
               (interpolate :newton (:newton state) x))]
    (vec (remove nil? [res1 res2]))))

(defn produce-outputs-for-alg
  "Считает выходы для одного алгоритма (linear/newton) и обновляет его стейт."
  [alg opts alg-state max-x]
  (if-not (alg-ready? alg opts alg-state)
    {:state alg-state
     :outputs []}
    (let [step    (:step opts)
          start-x (or (:next-x alg-state)
                      (some-> alg-state :points first :x))]
      (if (nil? start-x)
        {:state alg-state
         :outputs []}
        (loop [x    start-x
               outs []]
          (if (> x max-x)
            {:state   (assoc alg-state :next-x x)
             :outputs outs}
            (let [res  (interpolate alg alg-state x)
                  outs' (if res (conj outs res) outs)]
              (recur (+ x step) outs'))))))))

(defn produce-outputs
  "Считает выходы для всех включённых алгоритмов на отрезке [*, max-x]."
  [opts state max-x]
  (let [{lin-state :state lin-outs :outputs}
        (if (:linear? opts)
          (produce-outputs-for-alg :linear opts (:linear state) max-x)
          {:state (:linear state) :outputs []})

        {new-state :state new-outs :outputs}
        (if (:newton? opts)
          (produce-outputs-for-alg :newton opts (:newton state) max-x)
          {:state (:newton state) :outputs []})

        outputs (into lin-outs new-outs)]
    {:state   {:linear lin-state
               :newton new-state}
     :outputs outputs}))

(defn handle-datapoint
  [opts state point]
  (let [linear-state'
        (if (:linear? opts)
          (add-point :linear (:linear state) point)
          (:linear state))

        newton-state'
        (if (:newton? opts)
          (assoc (add-point :newton (:newton state) point)
                 :n (:n opts))
          (:newton state))

        ;; авто-очистка
        linear-state''
        (if (:linear? opts)
          (limit-state :linear opts linear-state')
          linear-state')

        newton-state''
        (if (:newton? opts)
          (limit-state :newton opts newton-state')
          newton-state')

        state' {:linear linear-state''
                :newton newton-state''}

        max-x (:x point)

        {:keys [state outputs]}
        (produce-outputs opts state' max-x)]
    {:state state
     :outputs outputs}))