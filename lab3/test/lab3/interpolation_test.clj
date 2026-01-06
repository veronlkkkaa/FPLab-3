(ns lab3.interpolation-test
  (:require [clojure.test :refer [deftest is testing]]
            [lab3.interpolation :as interp]))

; Линейная интерполяция
(deftest linear-basic-test
  (testing "Linear interpolation between two points"
    (let [points [{:x 0 :y 0}
                  {:x 10 :y 10}]]
      (is (= 5 (interp/compute-linear points 5)))
      (is (= 0 (interp/compute-linear points 0)))
      (is (= 10 (interp/compute-linear points 10))))))

(deftest linear-three-points-test
  (testing "Linear interpolation within a set of points"
    (let [points [{:x 0 :y 0}
                  {:x 2 :y 2}
                  {:x 4 :y 4}]]
      (is (= 3 (interp/compute-linear points 3))))))

; Разделённые разности
; y = x^2
(deftest divided-differences-test
  (testing "Divided differences for a quadratic function"
    (let [points [{:x 0 :y 0}
                  {:x 1 :y 1}
                  {:x 2 :y 4}]
          coeffs (interp/calc-coefficients points)]
      (is (= [0 1 1] coeffs)))))

; Ньютон
(deftest newton-eval-test
  (testing "Newton polynomial for x^2"
    (let [points [{:x 1 :y 1}
                  {:x 2 :y 4}
                  {:x 3 :y 9}]
          coeffs (interp/calc-coefficients points)]
      (is (=  4 (interp/newton-eval coeffs points 2)))
      (is (=  9 (interp/newton-eval coeffs points 3)))
      (is (= 16 (interp/newton-eval coeffs points 4))))))

(deftest choose-window-test
  (testing "Window choosing"
    (let [points (mapv (fn [x] {:x x :y x}) (range 10))]
      (is (= (subvec points 0 4)
             (interp/choose-window points 4 0)))
      (is (= (subvec points 3 7)
             (interp/choose-window points 4 4)))
      (is (= (subvec points 6 10)
             (interp/choose-window points 4 9))))))
