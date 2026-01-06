(ns lab3.properties-test
  (:require [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.generators :as gen]
            [lab3.interpolation :as interp]))

(def gen-safe-double
  (gen/double* {:NaN? false :infinite? false}))

(def gen-sorted-two-points
  (gen/fmap
   (fn [[x1 y1 x2 y2]]
     (let [[p1 p2] (sort-by :x [{:x x1 :y y1} {:x x2 :y y2}])]
       [p1 p2]))
   (gen/tuple gen-safe-double gen-safe-double
              gen-safe-double gen-safe-double)))

(def gen-sorted-three-points
  (gen/fmap
   (fn [[x1 y1 x2 y2 x3 y3]]
     (->> [{:x x1 :y y1}
           {:x x2 :y y2}
           {:x x3 :y y3}]
          (sort-by :x)
          vec))
   (gen/tuple gen-safe-double gen-safe-double
              gen-safe-double gen-safe-double
              gen-safe-double gen-safe-double)))

; проверяем совпадение с аналитикой

(defspec linear-exactness-test
  200
  (prop/for-all [[p1 p2] gen-sorted-two-points
                 x       gen-safe-double]
                (let [x1 (:x p1), y1 (:y p1)
                      x2 (:x p2), y2 (:y p2)]
                  (cond

                    (= x1 x2)
                    (= (interp/compute-linear [p1 p2] x1) y1)

                    (or (< x x1) (> x x2))
                    true

                    :else
                    (let [expected (+ y1 (* (/ (- x x1) (- x2 x1))
                                            (- y2 y1)))
                          actual   (interp/compute-linear [p1 p2] x)]
                      (< (Math/abs (- actual expected)) 1e-9))))))

; проверяем монотонность

(defspec linear-between-test
  200
  (prop/for-all [points gen-sorted-three-points]
                (let [[p1 p2 p3] points
                      x1 (:x p1)
                      x2 (:x p2)
                      x3 (:x p3)]

                  (if (and (= x1 x2) (= x2 x3))
                    true

                    (let [mid-x (/ (+ (:x p2) (:x p3)) 2)
                          y     (interp/compute-linear points mid-x)]
                      (and (<= (min (:y p2) (:y p3)) y)
                           (>= (max (:y p2) (:y p3)) y)))))))