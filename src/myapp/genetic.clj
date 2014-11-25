(ns myapp.genetic)

;(defn cosx [x] (Math/cos x))
;(defn sinx [x] (Math/sin x))

;constant data
(def POPULATION 100)
(def MEMBERS 13)                                             ;number of term and operator combine must be odd number
(def CHECK_POINTS 20)
(def INDEX 0)
(def MUTRATE 234)

;modified from stackoverflow to find even
(defn map-even [f coll]
  (map-indexed #(if (zero? (mod %1 2)) (f %2) %2) coll))

(defn map-odd [f coll]
  (map-indexed #(if (not (zero? (mod %1 2))) (f %2) %2) coll))

;get x number of input values to map to the random output (points to check).
(defn get-xindex [number step max]
  (take number (range 0 max step)))

;get NUMBER random values up to MAX (used for random values and random population[where x<4])
(defn get-random [number max]
  (take number (repeatedly #(rand-int max))))

(defn get-ones [number max]
  (take number (repeatedly #(rand-int 1))))

(defn add-random [x y]
  (+ (rand-int x) y))

;maps random numbers for terms and operands that within their ranges.
(defn map-ops-terms [ops terms]
  (map-odd #(add-random ops %) (map-even #(add-random terms %) (get-ones MEMBERS 3))))
;(map-odd #(add-random 3 %) (map-even #(add-random 13 %) (

;x is the size of the population
(defn get-population [size]
  (if (= size 1)
    (list (map-ops-terms 2 13))
    (cons (map-ops-terms 2 13) (get-population (dec size)))
    ))

;need to be able to choose actual values not just return the value!
(defn get-term [val, ind]
  ;(apply (nth '(#'cosx) 0) 0)
  ;(# (nth '(#'cosx #'sinx) ind) val)
  (cond
    (= 0 ind) val                                           ;x
    (= 1 ind) (Math/cos val)                                ;cosx
    (= 2 ind) (Math/sin val)                                ;sinx
    (= 3 ind) (Math/log val)                                ;lnx
    (= 12 ind) 0.25                                         ;fractional constants
    (= 13 ind) 0.5
    (= 14 ind) 0.75
    :else (- ind 2)                                         ;constants (2-9) - some #{} is "contains?"
    ))

;reuse power code from clojure pract project (TCO)
(defn power-tco [c, a]
  (loop [b c e a]
    (cond
      (> b 5000) b
      (= e 0) 1
      (= e 1) b
      :else (recur (* b c) (- e 1)))))

;list of possible operators
(defn get-operand [val1 val2 ind]
  ;(println val1 val2)
  (cond
    (= 0 ind) (* val1 val2)
    (= 1 ind) (+ val1 val2)
    (= 2 ind) (if (zero? val2) 5000 (/ val1 val2))          ;value that won't be matched when divide by zero
    (= 3 ind) (- val1 val2)
    (= 4 ind) (* val1 val2)
    ))

;combine 3 at index
;remove 3 items from the list and apply the operand to these terms. Use the x-value to calculate the terms.
(defn combine-3-at-index [x xs x-value]
  ;(println x xs)
  (let [l1 (take x xs) l2 (drop x xs)]
    (let [term1 (last l1) term2 (second l2)]
      (concat (butlast l1) (list (get-operand term1 term2 (first l2))) (drop 2 l2)))))

;from stackoverflow
(defn indices-of [f coll]
  (keep-indexed #(if (f %2) %1 nil) coll))

(defn first-index-of [f coll]
  (first (indices-of f coll)))

(defn find-thing [coll value]
  (first-index-of #(= % value) coll))

;applies the correct operand in order from the operator list.
(defn find-apply [termop x-value operator-list]
  (if (= 0 (count operator-list)) termop
      (if (= 1 (count termop))
        termop

        (let [operators (take-nth 2 (rest termop))]
          (let [mindex (find-thing operators (first operator-list))]

            (if (nil? mindex)
              (find-apply termop x-value (rest operator-list))
              (find-apply (combine-3-at-index (+ (* mindex 2) 1) termop x-value) x-value operator-list)
              )
            ;(apply operator for all parts and call find-and apply again. )
            )))))

;first call (find-and-apply termop x-values '(0 2 3 1)
;check that it isn't an even value...
(defn check-fitness [termop x-value]
  ;calculate terms ahead of time to speed things up and make things easier
  (first (find-apply (map-even #(get-term x-value %) termop) x-value '(4 0 2 3 1))))

;map (use pmap?) curried fx with population member for each x input
(defn compute-y [termop x-values]
  (map #(check-fitness termop %) x-values))

;create a vector of absolute errors between each computed y and actual y
(defn compute-errors [termop x-values y-values]
  (map #(max % (- %)) (map - (compute-y termop x-values) y-values)))

;returns the fitness score for the termop vector (population member)
(defn sum-errors [termop x-values y-values]
  (reduce + (compute-errors termop x-values y-values)))

;returns the fitness scores for each member of the population as a list.
(defn grade-population [population x-values y-values]
  (map #(sum-errors % x-values y-values) population))

(defn NaN? [x]
  (false? (== x x)))

(defn check-inf [x]
  (if (NaN? x)
    9000
    (if (Double/isInfinite x)
      9000
      x)))

(defn clean-scores [scores]
  (map check-inf scores))

(defn t-select [dirty-population]
  (let [score-population (clean-scores dirty-population) count (count dirty-population)]
    (loop [score-population score-population best 9000 random (rand-int count) chances count current 0]
      (if (= chances 0)
        current
        (if (< (nth score-population random) best)
          (recur score-population (nth score-population random) (rand-int count) (dec chances) random)
          (recur score-population best (rand-int count) (dec chances) current)
          )))))

;potentially too slow to use (we can return multiple indexes?)
(defn remove-item-by-index [x xs]
  (remove #{(nth xs x)} xs))

(defn remove-index-from-list [x xs]
  (let [x1 (+ x 1)]
    (concat (take x xs) (drop x1 xs))))

(defn cross [mom dad]
  (let [half (+ (/ (count mom) 2) 1)]                       ;two more than half
    (let [m1 (take half mom) m2 (drop half mom)
          d1 (take half dad) d2 (drop half dad)]
      (list (concat m1 d2) (concat d1 m2)))))

;single section cross over other options are (single point, multiple point, uniform)
(defn cross-over [scored-population population]
  (let [index (t-select scored-population)]
    (let [index2 (t-select (remove-index-from-list index scored-population))]
      (cross
        (nth population index)
        (nth (remove-index-from-list index population) index2)))))

(defn map-index [f coll n]
  (map-indexed #(if (zero? (mod %1 n)) (f %2) %2) coll))

(defn mutate2[x original]
  (if (== (rand-int 100) 1) (add-random x 1) original ))

(defn mutate2a [xss]
  (map-even #(mutate2 2 %) (map-odd #(mutate2 12 %) xss)))

;@number-generations - the number of times to recurse
(defn iterate-generation [number-generations x-values y-values population]
  (println x-values)
  (println y-values)
  (println population)

  (loop [generation number-generations x-values x-values y-values y-values ng number-generations
         scored-population (grade-population population x-values y-values)
         population population new-population '()]

    ;(println population)
    ;(println new-population)

    (if (= generation 0)
      population                                            ;return max in population.
      (if (= (count new-population) POPULATION)
        (do
          (println (apply min scored-population))
          (let [mutants (map mutate2a new-population)]
            (recur
              (dec generation)
              x-values
              y-values
              ng
              (grade-population mutants x-values y-values)
              mutants
              '()
              ))
          )

        ; (iterate-generation (dec ng) x-values y-values new-population)
        (recur
          generation
          x-values
          y-values
          ng
          scored-population
          population
          (concat (cross-over scored-population population) new-population)
          )))))

(defn initialize []
  ;random points (x,y) where x is spaced out and y is random
  (let [x (get-xindex CHECK_POINTS 2 30)                    ;get (0,1,2,3,4,5,6,7,8,9)
        y (get-random CHECK_POINTS 100)                     ;get 10 random numbers
        initial-population (get-population POPULATION)      ; map even cause terms are out of 14 not 3
        ]
    ;100 is sort of a random number of generations but just as a place-holder
    (concat (list  (first (iterate-generation 10 x y initial-population)) )(list x) (list y))))

(defn insert-odd [ind]
  (cond
    (= 0 ind) "*"
    (= 1 ind) "+"
    (= 2 ind) "/"          ;value that won't be matched when divide by zero
    (= 3 ind) "-"
    (= 4 ind) "^"                                           ; (power-tco val1 val2)
    ))

(defn insert-even [ind]
  (cond
    (= 0 ind) "x"                                           ;x
    (= 1 ind) "Math.cos(x)"                               ;cosx
    (= 2 ind) "Math.sin(x)"                                ;sinx
    (= 3 ind) "Math.log(x)"                                ;lnx
    (= 12 ind) "0.25"                                       ;fractional constants
    (= 13 ind) "0.5"
    (= 14 ind) "0.75"
    :else (str (- ind 2))                                          ;constants (2-9) - some #{} is "contains?"
    ))

(defn print-equation [eq]
 (map-odd #(insert-odd %) (map-even #(insert-even %) eq)))

;actual return statement to javascript so that we can visualize the graph
(defn print-population []
  ;"randomData = [{x:1,y:3},{x:2,y:7},{x:3,y:1}] function equation(x) { return cos(x)+5*x^2 }
  (let [data (initialize)]
    (println (print-equation (first data)))
    (str "ydata=[" (clojure.string/join "," (nth data 2))  "]; xdata=[" (clojure.string/join "," (second data))
         "]; function equation(x) { return " (apply str (print-equation (first data)))  "}")))