(ns myapp.genetic-test
  (:require [clojure.test :refer :all]
            [myapp.core :refer :all]))

(deftest end-to-end
  (testing "End to end testing"
    (myapp.genetic/initialize)))

(deftest test-fitness
  (testing "Composition of equations"
    (println (myapp.genetic/check-fitness-rec '(0 1 0) 1))
    (myapp.genetic/check-fitness-rec '(0 1 0) 1)

    (is (= 1  (myapp.genetic/check-fitness-rec '(0 1 0) 1) ));x / x = 1
    (is (= 1  (myapp.genetic/check-fitness-rec '(0 1 0) 2))) ;x / x = 1
    (is (= 4  (myapp.genetic/check-fitness-rec '(0 2 0) 2))) ;x + x = 4
    (is (= 6  (myapp.genetic/check-fitness-rec '(0 2 0 2 0) 2))) ;x + x + x = 6
    (is (= 9  (myapp.genetic/check-fitness-rec '(0 2 0 2 0) 3))) ;x + x + x = 9
    (is (= 4  (myapp.genetic/check-fitness-rec '(0 2 0 1 0) 3))) ;x + x / x = 4
    (is (= 12  (myapp.genetic/check-fitness-rec '(0 2 0 0 0) 3))) ;x + x * x = 12

    (is (= 2.6327476856711183  (myapp.genetic/check-fitness-rec '(1 2 1 2 1) 0.5))) ;cosx + cosx + cosx = 2.63

    (is (= 0.8775825618903728  (myapp.genetic/check-fitness-rec '(1) 0.5))) ; cosx = 0.878
    (is (= 0.5  (myapp.genetic/check-fitness-rec '(13) 0.5))) ; 0.5
    (is (= 0.4387912809451864  (myapp.genetic/check-fitness-rec '(13 0 1) 0.5))) ; 0.5 * cosx = 2.63
    (is (= 2.6327476856711183  (myapp.genetic/check-fitness-rec '(5 0 1) 0.5))) ;3 * cosx = 2.63
  ))

(deftest test-computed-y-values
  (testing "Comute y-values"
    (is (= '(3 6 9 12 15 18) (myapp.genetic/compute-y '(0 2 0 2 0) '(1 2 3 4 5 6))))
    (is (= '(3 15 30 45 60 75) (myapp.genetic/compute-y '(0 2 0 2 0) '(1 5 10 15 20 25))))
  ))

(deftest test-map-errors
  (testing "Determining errors between computed y and actual y"
    (is (= '(0 0 0 0 0 0) (myapp.genetic/compute-errors '(0 2 0 2 0) '(1 2 3 4 5 6) '(3 6 9 12 15 18))))
    (is (= '(0 0 2 1 0 0) (myapp.genetic/compute-errors '(0 2 0 2 0) '(1 2 3 4 5 6) '(3 6 11 13 15 18)))) ;+2 +1
    (is (= '(0 0 0 1 0 0) (myapp.genetic/compute-errors '(0 2 0 2 0) '(1 2 3 4 5 6) '(3 6 9 13 15 18)))) ;+1
    (is (= '(0 0 0 1 0 0) (myapp.genetic/compute-errors '(0 2 0 2 0) '(1 2 3 4 5 6) '(3 6 9 11 15 18)))) ;-1 (test abs)
  ))

;sums the test-map-errors to receive fitness score (0 is best)
(deftest test-member-error-score
  (testing "Sum error scores for each member"
    (is (= 0 (myapp.genetic/sum-errors '(0 2 0 2 0) '(1 2 3 4 5 6) '(3 6 9 12 15 18))))
    (is (= 3 (myapp.genetic/sum-errors '(0 2 0 2 0) '(1 2 3 4 5 6) '(3 6 11 13 15 18))))
    (is (= 1 (myapp.genetic/sum-errors '(0 2 0 2 0) '(1 2 3 4 5 6) '(3 6 9 13 15 18)))) ;+1
    (is (= 1 (myapp.genetic/sum-errors '(0 2 0 2 0) '(1 2 3 4 5 6) '(3 6 9 11 15 18)))) ;-1 (test abs)

    ;x+x*x = x^2+x
    (is (= 0 (myapp.genetic/sum-errors '(0 2 0 0 0) '(1 2 3 4 5 6) '(2 6 12 20 30 42))))
    ;1+(1*1)=2, 2+(2*2)=6, 3+(3*3)=12, 4+(4*4)=20, 5+(5*5)=30, 6+(6*6)=42, series is incr by previous incr + 2
  ))

(deftest test-population-fitness
  (testing "Computing fitness (sum of error) for each member of a population"
    ;(2 6 12 20 30 42), (3 6 9 12 15 18) so errors are (1 0 1 7 15 24), (0 0 2 1 0 0) so sum errors is (48), (3)
    (is (= '(48 3) (myapp.genetic/grade-population '((0 2 0 0 0 ) (0 2 0 2 0)) '(1 2 3 4 5 6) '(3 6 11 13 15 18))))
  ))

(end-to-end)
(test-fitness)
(test-computed-y-values)
(test-map-errors)
(test-member-error-score)
(test-population-fitness)