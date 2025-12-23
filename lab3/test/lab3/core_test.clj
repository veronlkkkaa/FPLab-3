(ns lab3.core-test
  (:require [clojure.test :refer [deftest is testing]]
            [lab3.core :as core]
            [lab3.interpolation :as interpolator]))

(deftest extract-point-test
  (testing "Parsing various input formats"
    (is (= {:x 1.0 :y 2.0} (core/extract-point-from-line "1 2")))
    (is (= {:x 1.0 :y 2.0} (core/extract-point-from-line "1;2")))
    (is (= {:x 1.0 :y 2.0} (core/extract-point-from-line "1,2")))
    (is (nil? (core/extract-point-from-line "")))
    (is (nil? (core/extract-point-from-line "abc def")))))

(deftest linear-stream-processing-test
  (testing "Stream processing with linear interpolation"
    (let [config {:linear? true :newton? false :step 1 :n 4}
          initial-state (interpolator/create-initial-state)

          ;; Добавляем первую точку
          result1 (interpolator/handle-new-point config initial-state {:x 0 :y 0})
          normalized-outputs1 (map #(-> %
                                       (update :x interpolator/sanitize-zero)
                                       (update :y interpolator/sanitize-zero))
                                  (:outputs result1))]

      (is (= [] normalized-outputs1))

      ;; Добавляем вторую точку
      (let [result2 (interpolator/handle-new-point config (:state result1) {:x 1 :y 1})
            normalized-outputs2 (map #(-> %
                                         (update :x interpolator/sanitize-zero)
                                         (update :y interpolator/sanitize-zero))
                                    (:outputs result2))]

        (is (= [{:alg :linear :x 0.0 :y 0.0}
                {:alg :linear :x 1.0 :y 1.0}]
               normalized-outputs2))))))
