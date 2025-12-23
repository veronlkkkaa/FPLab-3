(ns lab3.interpolation-test
  (:require [clojure.test :refer [deftest is testing]]
            [lab3.interpolation :as interpolator]))

;; Тесты линейной интерполяции

(deftest basic-linear-interpolation-test
  (testing "Linear interpolation calculation between two points"
    (let [data-points [{:x 0 :y 0}
                       {:x 10 :y 10}]]
      (is (= 5 (interpolator/calculate-linear-interpolation data-points 5)))
      (is (= 0 (interpolator/calculate-linear-interpolation data-points 0)))
      (is (= 10 (interpolator/calculate-linear-interpolation data-points 10))))))

(deftest multi-point-linear-interpolation-test
  (testing "Linear interpolation within multiple points"
    (let [data-points [{:x 0 :y 0}
                       {:x 2 :y 2}
                       {:x 4 :y 4}]]
      (is (= 3 (interpolator/calculate-linear-interpolation data-points 3))))))

;; Тесты таблицы разделенных разностей
;; Для функции y = x^2

(deftest difference-table-test
  (testing "Building difference table for quadratic function"
    (let [data-points [{:x 0 :y 0}
                       {:x 1 :y 1}
                       {:x 2 :y 4}]
          coefficients (interpolator/build-difference-table data-points)]
      (is (= [0 1 1] coefficients)))))

;; Тесты интерполяции Ньютона

(deftest newton-polynomial-evaluation-test
  (testing "Newton polynomial evaluation for x^2"
    (let [data-points [{:x 1 :y 1}
                       {:x 2 :y 4}
                       {:x 3 :y 9}]
          coefficients (interpolator/build-difference-table data-points)]
      (is (=  4 (interpolator/evaluate-newton-polynomial coefficients data-points 2)))
      (is (=  9 (interpolator/evaluate-newton-polynomial coefficients data-points 3)))
      (is (= 16 (interpolator/evaluate-newton-polynomial coefficients data-points 4))))))

(deftest window-selection-test
  (testing "Selecting optimal window for interpolation"
    (let [data-points (mapv (fn [x] {:x x :y x}) (range 10))]
      (is (= (subvec data-points 0 4)
             (interpolator/select-optimal-window data-points 4 0)))
      (is (= (subvec data-points 3 7)
             (interpolator/select-optimal-window data-points 4 4)))
      (is (= (subvec data-points 6 10)
             (interpolator/select-optimal-window data-points 4 9))))))
