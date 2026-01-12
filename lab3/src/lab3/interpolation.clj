(ns lab3.interpolation)

(defn limit-queue
  "Обрезает очередь до max-size, выкидывая элементы слева."
  [q max-size]
  (loop [q q]
    (if (<= (count q) max-size)
      q
      (recur (pop q)))))

;; Полиморфизм

(defmulti interpolate
  "Главный полиморфный интерфейс для интерполяции.
   Диспетчеризация: [операция алгоритм]
   
   Операции:
   - :compute     - вычислить значение интерполяции
   - :process     - обработать точку
   - :ready?      - проверить готовность
   - :max-points  - получить макс. кол-во точек для хранения"
  (fn [operation alg & _] [operation alg]))

;; Stream processing helper

(defn produce-outputs-for-alg
  "Считает выходы для одного алгоритма и обновляет его стейт."
  [alg opts alg-state max-x]
  (if-not (interpolate :ready? alg alg-state opts)
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
            (let [points (:points alg-state)
                  n      (:n alg-state)
                  y      (if (= alg :linear)
                           (interpolate :compute alg points x)
                           (interpolate :compute alg points n x))
                  res    (when y {:alg alg :x x :y y})
                  outs'  (if res (conj outs res) outs)]
              (recur (+ x step) outs'))))))))

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

(defmethod interpolate [:compute :linear]
  [_ _ points x]
  (when-let [[p1 p2] (find-segment points x)]
    (let [x1 (:x p1)
          y1 (:y p1)
          x2 (:x p2)
          y2 (:y p2)]
      (if (= x1 x2)
        y1
        (let [t (/ (- x x1) (- x2 x1))]
          (+ y1 (* t (- y2 y1))))))))

(defmethod interpolate [:ready? :linear]
  [_ _ state _opts]
  (>= (count (:points state)) 2))

(defmethod interpolate [:max-points :linear]
  [_ _ _opts]
  2)

(defmethod interpolate [:process :linear]
  [_ alg opts state point]
  (let [state' (-> state
                   (update :points conj point)
                   (update :points limit-queue
                           (interpolate :max-points alg opts)))
        {:keys [state outputs]}
        (produce-outputs-for-alg alg opts state' (:x point))]
    {:state state
     :outputs outputs}))

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

(defmethod interpolate [:compute :newton]
  [_ _ points n x]
  (let [window (choose-window points n x)
        coeffs (calc-coefficients window)]
    (newton-eval coeffs window x)))

(defmethod interpolate [:ready? :newton]
  [_ _ state opts]
  (>= (count (:points state)) (:n opts)))

(defmethod interpolate [:max-points :newton]
  [_ _ opts]
  (inc (:n opts)))

(defmethod interpolate [:process :newton]
  [_ alg opts state point]
  (let [state' (-> state
                   (update :points conj point)
                   (assoc :n (:n opts))
                   (update :points limit-queue
                           (interpolate :max-points alg opts)))
        {:keys [state outputs]}
        (produce-outputs-for-alg alg opts state' (:x point))]
    {:state state
     :outputs outputs}))

  ;; State management

(defn normalize-zero [x]
  (let [d (double x)]
    (if (= d -0.0) 0.0 d)))

(defn init-state []
  {:linear {:points clojure.lang.PersistentQueue/EMPTY
            :next-x nil}
   :newton {:points clojure.lang.PersistentQueue/EMPTY
            :next-x nil}})

(defn handle-datapoint
  "Обрабатывает входящую точку для всех активных алгоритмов"
  [opts state point]
  (let [{lin-state :state lin-outs :outputs}
        (if (:linear? opts)
          (interpolate :process :linear opts (:linear state) point)
          {:state (:linear state) :outputs []})

        {new-state :state new-outs :outputs}
        (if (:newton? opts)
          (interpolate :process :newton opts (:newton state) point)
          {:state (:newton state) :outputs []})

        outputs (into lin-outs new-outs)]
    {:state   {:linear lin-state
               :newton new-state}
     :outputs outputs}))
