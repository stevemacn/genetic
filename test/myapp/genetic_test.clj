(ns myapp.genetic-test
  (:require [clojure.test :refer :all]
            [myapp.core :refer :all]))

(deftest end-to-end
  (testing "End to end testing"
    (myapp.genetic/initialize)))

(deftest test-fitness
  (testing "Composition of equations"

    ;'(0 2 3 1) denotes the operator precedences
    (is (= 1   (myapp.genetic/check-fitness '(0 2 0) 1)));x / x = 1
    (is (= 1   (myapp.genetic/check-fitness '(0 2 0) 2))) ;x / x = 1
    (is (= 4   (myapp.genetic/check-fitness '(0 1 0) 2))) ;x + x = 4
    (is (= 6   (myapp.genetic/check-fitness '(0 1 0 1 0) 2))) ;x + x + x = 6
    (is (= 9   (myapp.genetic/check-fitness '(0 1 0 1 0) 3))) ;x + x + x = 9
    (is (= 4   (myapp.genetic/check-fitness '(0 1 0 2 0) 3))) ;x + x / x = 4
    (is (= 12  (myapp.genetic/check-fitness '(0 1 0 0 0) 3))) ;x + x * x = 12
    (is (= 12  (myapp.genetic/check-fitness '(0 0 0 1 0) 3))) ;x * x + x = 12
    (is (= 42  (myapp.genetic/check-fitness '(0 1 0 0 0) 6))) ;x + x * x = 42
    (is (= 4.7917594692280545  (myapp.genetic/check-fitness '(6 1 11 3 7 0 4 1 3) 6))) ;3+lnx

    ;.57 msecs
    (time (myapp.genetic/check-fitness '(0 0 0 1 0 1 0 3 0 3 0) 6)) ;x * x + x + x - x - x= 48

    (is (= 36  (myapp.genetic/check-fitness '(0 0 0 1 0 1 0 3 0 3 0) 6))) ;x * x + x + x - x - x= 48

    (is (= 2.6327476856711183  (myapp.genetic/check-fitness '(1 1 1 1 1) 0.5))) ;cosx + cosx + cosx = 2.63

    (is (= 0.8775825618903728  (myapp.genetic/check-fitness '(1) 0.5))) ; cosx = 0.878
    (is (= 0.5  (myapp.genetic/check-fitness '(13) 0.5))) ; 0.5
    (is (= 0.4387912809451864  (myapp.genetic/check-fitness '(13 0 1) 0.5))) ; 0.5 * cosx = 2.63
    (is (= 2.6327476856711183  (myapp.genetic/check-fitness '(5 0 1) 0.5))) ;3 * cosx = 2.63
  ))

(deftest test-computed-y-values
  (testing "Comute y-values"
    (is (= '(3 6 9 12 15 18) (myapp.genetic/compute-y '(0 1 0 1 0) '(1 2 3 4 5 6))))
    (is (= '(5 5 5 5 5 5) (myapp.genetic/compute-y '(0 1 7 3 0) '(1 2 3 4 5 6))))
    (is (= '(3 15 30 45 60 75) (myapp.genetic/compute-y '(0 1 0 1 0) '(1 5 10 15 20 25))))
  ))

(deftest test-map-errors
  (testing "Determining errors between computed y and actual y"
    (is (= '(0 0 0 0 0 0) (myapp.genetic/compute-errors '(0 1 0 1 0) '(1 2 3 4 5 6) '(3 6 9 12 15 18))))
    (is (= '(0 0 2 1 0 0) (myapp.genetic/compute-errors '(0 1 0 1 0) '(1 2 3 4 5 6) '(3 6 11 13 15 18)))) ;+2 +1
    (is (= '(0 0 0 1 0 0) (myapp.genetic/compute-errors '(0 1 0 1 0) '(1 2 3 4 5 6) '(3 6 9 13 15 18)))) ;+1
    (is (= '(0 0 0 1 0 0) (myapp.genetic/compute-errors '(0 1 0 1 0) '(1 2 3 4 5 6) '(3 6 9 11 15 18)))) ;-1 (test abs)
    (is (= '(2 4 6 7 10 12) (myapp.genetic/compute-errors '(0 1 0 3 0) '(1 2 3 4 5 6) '(3 6 9 11 15 18)))) ;;x+x-x = x so then
    (is (= '(3 3/2 13/3 23/4 9 67/6) (myapp.genetic/compute-errors '(0 1 7 2 0) '(1 2 3 4 5 6) '(3 6 9 11 15 18)))) ; =x+4*x = 5x
  ))


;sums the test-map-errors to receive fitness score (0 is best)
(deftest test-member-error-score
  (testing "Sum error scores for each member"
    (is (= 0 (myapp.genetic/sum-errors '(0 1 0 1 0) '(1 2 3 4 5 6) '(3 6 9 12 15 18))))
    (is (= 3 (myapp.genetic/sum-errors '(0 1 0 1 0) '(1 2 3 4 5 6) '(3 6 11 13 15 18))))
    (is (= 1 (myapp.genetic/sum-errors '(0 1 0 1 0) '(1 2 3 4 5 6) '(3 6 9 13 15 18)))) ;+1
    (is (= 1 (myapp.genetic/sum-errors '(0 1 0 1 0) '(1 2 3 4 5 6) '(3 6 9 11 15 18)))) ;-1 (test abs)
    (is (= 41 (myapp.genetic/sum-errors '(0 1 0 3 0) '(1 2 3 4 5 6) '(3 6 9 11 15 18)))) ;-1 (test abs)
    (is (= 139/4 (myapp.genetic/sum-errors '(0 1 7 2 0) '(1 2 3 4 5 6) '(3 6 9 11 15 18)))) ;-1 (test abs)

    ;x+x*x = x^2+x
    (is (= 0 (myapp.genetic/sum-errors '(0 1 0 0 0) '(1 2 3 4 5 6) '(2 6 12 20 30 42))))
    ;1+(1*1)=2, 2+(2*2)=6, 3+(3*3)=12, 4+(4*4)=20, 5+(5*5)=30, 6+(6*6)=42, series is incr by previous incr + 2
  ))

(deftest test-population-fitness
  (testing "Computing fitness (sum of error) for each member of a population"
    ;(2 6 12 20 30 42), (3 6 9 12 15 18) so errors are (1 0 1 7 15 24), (0 0 2 1 0 0) so sum errors is (48), (3)
    (is (= '(48 3) (myapp.genetic/grade-population '((0 1 0 0 0 ) (0 1 0 1 0)) '(1 2 3 4 5 6) '(3 6 11 13 15 18))))
    (is (= '(48 3 48 3 48 3 48 3 48 3 48 3) (myapp.genetic/grade-population '(
         (0 1 0 0 0 ) (0 1 0 1 0) (0 1 0 0 0 ) (0 1 0 1 0) (0 1 0 0 0 ) (0 1 0 1 0)
         (0 1 0 0 0 ) (0 1 0 1 0) (0 1 0 0 0 ) (0 1 0 1 0) (0 1 0 0 0 ) (0 1 0 1 0)
       ) '(1 2 3 4 5 6) '(3 6 11 13 15 18))))

    ;runs in 0.022 msecs
    ;how to analyze stack size? stable for 540 members
    #_
    (time (myapp.genetic/grade-population '(
        (0 2 0 0 0 ) (0 2 0 2 0) (0 2 0 0 0 ) (0 2 0 2 0) (0 2 0 0 0 ) (0 2 0 2 0) ;6
        (0 2 0 0 0 ) (0 2 0 2 0) (0 2 0 0 0 ) (0 2 0 2 0) (0 2 0 0 0 ) (0 2 0 2 0) ;12
        (0 2 0 0 0 ) (0 2 0 2 0) (0 2 0 0 0 ) (0 2 0 2 0) (0 2 0 0 0 ) (0 2 0 2 0) ;18
        (0 2 0 0 0 ) (0 2 0 2 0) (0 2 0 0 0 ) (0 2 0 2 0) (0 2 0 0 0 ) (0 2 0 2 0) ;24
        (0 2 0 0 0 ) (0 2 0 2 0) (0 2 0 0 0 ) (0 2 0 2 0) (0 2 0 0 0 ) (0 2 0 2 0) ;30
        (0 2 0 0 0 ) (0 2 0 2 0) (0 2 0 0 0 ) (0 2 0 2 0) (0 2 0 0 0 ) (0 2 0 2 0) ;36
        (0 2 0 0 0 ) (0 2 0 2 0) (0 2 0 0 0 ) (0 2 0 2 0) (0 2 0 0 0 ) (0 2 0 2 0) ;42
        (0 2 0 0 0 ) (0 2 0 2 0) (0 2 0 0 0 ) (0 2 0 2 0) (0 2 0 0 0 ) (0 2 0 2 0) ;48
        (0 2 0 0 0 ) (0 2 0 2 0) (0 2 0 0 0 ) (0 2 0 2 0) (0 2 0 0 0 ) (0 2 0 2 0) ;54
        (0 2 0 0 0 ) (0 2 0 2 0) (0 2 0 0 0 ) (0 2 0 2 0) (0 2 0 0 0 ) (0 2 0 2 0) ;60
        (0 2 0 0 0 ) (0 2 0 2 0) (0 2 0 0 0 ) (0 2 0 2 0) (0 2 0 0 0 ) (0 2 0 2 0) ;66
        (0 2 0 0 0 ) (0 2 0 2 0) (0 2 0 0 0 ) (0 2 0 2 0) (0 2 0 0 0 ) (0 2 0 2 0) ;72
        (0 2 0 0 0 ) (0 2 0 2 0) (0 2 0 0 0 ) (0 2 0 2 0) (0 2 0 0 0 ) (0 2 0 2 0) ;78
        (0 2 0 0 0 ) (0 2 0 2 0) (0 2 0 0 0 ) (0 2 0 2 0) (0 2 0 0 0 ) (0 2 0 2 0) ;84
        (0 2 0 0 0 ) (0 2 0 2 0) (0 2 0 0 0 ) (0 2 0 2 0) (0 2 0 0 0 ) (0 2 0 2 0) ;90
      ) '(1 2 3 4 5 6) '(3 6 11 13 15 18)))
    ))

(deftest test-tuple-generator
  (testing "Checking that tuples are created from score and population and sorted correctly"
    (println
    (is (=
          '((0 (0 1 0)) (3 (1 0 1)) (22 (2 1 3)))
          (myapp.genetic/sort-population-tuples '(0 22 3) '((0 1 0) (2 1 3) (1 0 1))))))))

#_
(deftest test-remove-item
  (testing "removing item from a list"
    (time (myapp.genetic/remove-item 3 '(1 2 0 4)))
    (println (myapp.genetic/remove-item 3 '(1 2 0 4)))      ;searches for item not actual index.
    ;likely too slow 0.05 - 0.07 msecs
    ))

;.06 - .09 msec (not significantly slower than remove)
(deftest test-cross
  (testing "check crossing two parents"
    (time (myapp.genetic/cross '(1 1 2 2 3 1) '(1 1 2 2 3 1)))
    (is (= '((1 1 2 2 3) (0 1 0 1 1)) (myapp.genetic/cross '(1 1 2 2 1) '(0 1 0 1 3))))
  ))

;.05 - .08 remove index from list is faster.
(deftest test-remove-item
  (testing "removing item from a list"
    (println "time to remove from list")
    (time (myapp.genetic/remove-index-from-list 3 '(0 1 2 3 4) ))
    (time (myapp.genetic/remove-item-by-index 3 '(0 1 2 3 4) ))
    (is (= '(0 1 2 4) (myapp.genetic/remove-item-by-index 3 '(0 1 2 5 4))))
    (is (= '(0 1 2 4) (myapp.genetic/remove-index-from-list 3 '(0 1 2 5 4))))
    ))

;48 3 48, (0 2 0 0 0) and (0 2 0 2 0) sent to myapp
(deftest test-crossover
  (testing "check the crossover"
    (println "near end to end testing")
    ;0 2 0 0 0
    ;t 0 t 0 t
    ;0 0 0 2 0
    ;x+x*x x+x+x x*x+x
    ;still fails cause grade pop relies on broken get fit blah blah.
    (let [population '((0 1 0 0 0) (0 1 0 1 0) (0 1 7 2 0) (0 0 0 1 0) (1 1 0 1 1)) xvals '(1 2 3 4 5 6) yvals '(3 6 11 13 15 18)]
      (println (myapp.genetic/grade-population population xvals yvals))
      (println (myapp.genetic/cross-over (myapp.genetic/grade-population population xvals yvals) population))
      (myapp.genetic/cross-over (myapp.genetic/grade-population population xvals yvals) population)
    )
    ))

(deftest test-remove-3
  (testing "removing three from list applying operator and continue"
       ;fix tests they fail because they no longer calculate terms. No rush - this is covered by other tests now.
    (is (= '(0 1 4 1 3 0) (myapp.genetic/combine-3-at-index 3 '(0 1 0 1 0 1 3 0) 2))) ;x+x = 2+2
    (is (= '(0 1 2 1 3 0) (myapp.genetic/combine-3-at-index 3 '(0 1 0 1 0 1 3 0) 1))) ;x+x = 1+1
    (is (= '(0 1 0 1 2.6931471805599454 0) (myapp.genetic/combine-3-at-index 5 '(0 1 0 1 0 1 3 0) 2))) ;x+logx = 2+log2
    ))

(end-to-end)
(test-fitness)
(test-computed-y-values)
(test-map-errors)
(test-member-error-score)
(test-population-fitness)
;(test-tuple-generator)
(test-remove-item)
(test-cross)
(test-crossover)
(test-remove-3)
;(estimate-time)