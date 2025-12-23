(ns lab3.properties-test
  (:require [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.generators :as gen]
            [lab3.interpolation :as interpolator]))

;; Генераторы тестовых данных

(def generate-valid-double
  "Генератор валидных чисел (без NaN и бесконечностей)"
  (gen/double* {:NaN? false :infinite? false}))

(def generate-two-sorted-points
  "Генератор двух отсортированных точек"
  (gen/fmap
   (fn [[x1 y1 x2 y2]]
     (let [[pt1 pt2] (sort-by :x [{:x x1 :y y1} {:x x2 :y y2}])]
       [pt1 pt2]))
   (gen/tuple generate-valid-double generate-valid-double
              generate-valid-double generate-valid-double)))

(def generate-three-sorted-points
  "Генератор трёх отсортированных точек"
  (gen/fmap
   (fn [[x1 y1 x2 y2 x3 y3]]
     (->> [{:x x1 :y y1}
           {:x x2 :y y2}
           {:x x3 :y y3}]
          (sort-by :x)
          vec))
   (gen/tuple generate-valid-double generate-valid-double
              generate-valid-double generate-valid-double
              generate-valid-double generate-valid-double)))

;; Property-based тесты

(defspec linear-interpolation-accuracy-test
  200
  (prop/for-all [[pt1 pt2] generate-two-sorted-points
                 target-x  generate-valid-double]
                (let [{x1 :x y1 :y} pt1
                      {x2 :x y2 :y} pt2]
                  (cond
                    ;; Случай совпадающих x координат
                    (= x1 x2)
                    (= (interpolator/calculate-linear-interpolation [pt1 pt2] x1) y1)

                    ;; Точка вне интервала - пропускаем
                    (or (< target-x x1) (> target-x x2))
                    true

                    ;; Проверяем точность интерполяции
                    :else
                    (let [expected-value (+ y1 (* (/ (- target-x x1) (- x2 x1))
                                                  (- y2 y1)))
                          actual-value (interpolator/calculate-linear-interpolation
                                        [pt1 pt2]
                                        target-x)]
                      (< (Math/abs (- actual-value expected-value)) 1e-9))))))

(defspec linear-interpolation-bounds-test
  200
  (prop/for-all [data-points generate-three-sorted-points]
                (let [[pt1 pt2 pt3] data-points
                      {x1 :x} pt1
                      {x2 :x} pt2
                      {x3 :x} pt3]

                  (if (and (= x1 x2) (= x2 x3))
                    ;; Все x одинаковые - пропускаем
                    true

                    ;; Проверяем, что интерполированное значение между границами
                    (let [middle-x (/ (+ (:x pt2) (:x pt3)) 2)
                          interpolated-y (interpolator/calculate-linear-interpolation
                                          data-points
                                          middle-x)
                          min-y (min (:y pt2) (:y pt3))
                          max-y (max (:y pt2) (:y pt3))]
                      (and (<= min-y interpolated-y)
                           (>= max-y interpolated-y)))))))