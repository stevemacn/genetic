(ns myapp.genetic)

;(defn cosx [x] (Math/cos x))
;(defn sinx [x] (Math/sin x))

;constant data
(def POPULATION 5)
(def MEMBERS 9)                                             ;number of term and operator combine must be odd number
(def CHECK_POINTS 10)
(def INDEX 0)

;get x number of input values to map to the random output (points to check).
(defn get-xindex [number step max]
  (take number (range 0 max step)))

;get NUMBER random values up to MAX (used for random values and random population[where x<4])
(defn get-random [number max]
  (take number (repeatedly #(rand-int max))))

;x is the size of the population
(defn get-population [size]
  (if (= size 1)
    (list (get-random MEMBERS 3))
    (cons (get-random MEMBERS 3) (get-population (dec size)))
    ))

;need to be able to choose actual values not just return the value!
(defn get-term [val, ind]
  ;(apply (nth '(#'cosx) 0) 0)
  ;(# (nth '(#'cosx #'sinx) ind) val)
  (cond
    (= 0 ind) val                                           ;x
    (= 1 ind) (Math/cos val)                                ;cosx
    (= 2 ind) (Math/sin val)                                ;sinx
    (= 3 ind) (Math/log val)                                ;logx
    (= 12 ind) 0.25                                         ;fractional constants
    (= 13 ind) 0.5
    (= 14 ind) 0.75
    :else (- ind 2)                                         ;constants (2-9) - some #{} is "contains?"
    ))

;reuse power code from clojure pract project (TCO)
(defn power-tco [c, a]
  (loop [b c e a]
    (cond
      (= e 0) 1
      (= e 1) b
      :else (recur (* b c) (- e 1)))))

;list of possible operators
(defn get-operand [val1 val2 ind]
  (cond
    (= 0 ind) (* val1 val2)
    (= 1 ind) (/ val1 val2)
    (= 2 ind) (+ val1 val2)
    (= 3 ind) (- val1 val2)
    (= 4 ind) (power-tco val1 val2)))

;going to take a fair amount to rewrite this using TCO (should be recursing on op not final term).
;returns the equation-calculated value of y for an x value. (needs to be fixed currently looking at array of x not 1 x)
(defn check-fitness-rec [termop x-value]
  (if (empty? termop)
    0                                                       ;find a better error message
    (let [size (count termop) rest-terms (rest termop)]
      (if
        (= size 1) (get-term x-value (first termop))
                   (get-operand
                     (get-term x-value (first termop))      ;first term
                     (check-fitness-rec (rest rest-terms) x-value) ;second term
                     (first rest-terms)                     ;operand
                     )))))
;refactor check-fitness-rec to assemble equations

;map (use pmap?) curried fx with population member for each x input
(defn compute-y [termop x-values]
  (map #(check-fitness-rec termop %) x-values))

;create a vector of absolute errors between each computed y and actual y
(defn compute-errors [termop x-values y-values]
  (map #(Math/abs %) (map - (compute-y termop x-values) y-values)))

;returns the fitness score for the termop vector (population member)
(defn sum-errors [termop x-values y-values]
  (reduce + (compute-errors termop x-values y-values)))

;returns the fitness scores for each member of the population as a list.
(defn grade-population [population x-values y-values]
  (map #(sum-errors % x-values y-values) population))

;potentially too slow to use (we can return multiple indexes?)
(defn remove-item [x xs]
  (remove #{x} xs))

(defn cross [mom dad]
  (let [half (+ (/ (count mom) 2) 1)]                       ;two more than half
    (let [m1 (take half mom) m2 (drop half mom)
          d1 (take half dad) d2 (drop half dad)]
      (println (concat d1 m2))
      (println (concat m1 d2))

      (list (concat m1 d2) (concat d1 m2)))))

;create new generation from parents
;may want to investigate multiple types of crossover (single point, multiple point, uniform)
(defn cross-over [scored-population population]
  ;(let [indexes (population-select scored-population)]

  ;(cross
  ;  (nth (first indexes) population)
  ;  (nth (second indexes) population)
  ;
  ; ))
  )



;randomly mutate member of the population
(defn mutation [member])
;we need to map this to the whole population (map mutation population)

;arithmetic, heurstic crossover
(defn pair-parents [mom dad]

  )

;creates a list of sorted tuples of the population (score, member)
(defn sort-population-tuples [scored-population population]
  (let [population-tuples (map list scored-population population) half (/ (count population) 2)]
    ;return the better half or ???
    0
    ;(take half (iterate max population-tuples))
    ;(sort-by first population-tuples) CRAZY SLOW! What can we do instead?
    ))

;tournament select, roulette?, fitness proportionate
;currently stochastic
;how is this returning new data
;(defn population-select [population x-values y-values]
;(let [scored-population (grade-population population x-values y-values)]
; (sort-population-tuples scored-population population)
;we now have sorted population tuples which can be crossed and mutated.

;remove index for scored-population
;get corresponding pop value (remove him too).
;create new members
;concat new members with rest of scored population.
; )


;get random value
;(let [size (- (count population) 1)]
;(let [index1 (random-int size) index2 (random-int size)]
;(if (= index1 index2)
; (pair-parents (nth population index1) (nth population (- index1 1)))
; (pair-parents (nth population index1) (nth population index2))
;))))

;recursively iterates through the populations - should be a generator
;so given a population and goal (y) it should continue for number-generations
;and then return the current best species to be visualized in D3
;@number-generations - the number of times to recurse
(defn iterate-generation [number-generations x-values y-values population]
  (println x-values)
  (println y-values)
  (println population)

  ;(if (= number-generations 0)
  ; (print-population population)                           ;highest ranked member (sorted by rank?)
  ;refactor to loop - recur
  ; (iterate-generation (dec number-generations) x-values y-values (population-select population x-values y-values))
  ;)

  ; grade-equation[x y population]
  ; sort (tuples from grade-equation)
  ; cross over and mutations
  ; iterate((dec number-generations) x y new-population) ;where new pop comes from mutations/crossover fitness etc.
  )

(defn initialize []
  ;random points (x,y) where x is spaced out and y is random
  (let [x (get-xindex CHECK_POINTS 1 10)                    ;get (0,1,2,3,4,5,6,7,8,9)
        y (get-random CHECK_POINTS 100)                     ;get 10 random numbers
        initial-population (get-population POPULATION)
        ]

    ;100 is sort of a random number of generations but just as a place-holder
    (iterate-generation 100 x y initial-population)
    ))

;actual return statement to javascript so that we can visualize the graph
(defn print-population [equation data-values xind]
  ;"randomData = [{x:1,y:3},{x:2,y:7},{x:3,y:1}] function equation(x) { return cos(x)+5*x^2 }
  (str "randomData=" data-values ";" "xind=" xind "; function equation(x) { return" "Math.cos(x)+3*x^2" "}")
  "Math.cos(x)+3*x^2"                                       ;this is x
  )

;generator or all at once? we only support one user - so could be saved in memory.
(defn get-data [] "input=[1,2,3,4,5,6,7,8,9,10]; arr=[];
 for (i in input) arr.push(Math.cos(i)); console.log(arr)")

;sample return value for javascript
;we return the randomData we tried to fit
;we return the equation we found that most closely fit the data

;correct the value from score-population
(defn correct-fitness [x]
      (cond
        (< 100 x) 0                                             ;if x is greater than 100 result 0
        (> 0 x) 0                                     ;if x is smaller than 0 result 0
        :else (- 100 x)
        )
      )

;total fitness, should return the sum of score-population
(defn total-fitness [score-population]
      (cond
        (== (count score-population) 0) ()
        (== (count score-population) 1) (correct-fitness (first score-population))
        :else (+ (correct-fitness (first score-population)) (total-fitness (rest score-population)))
        )
      )

;this function must be called before used of selection function each time
;(def fSlice (rand-int (total-fitness score-population)))
((fn [fSlice] (rand-int fSlice)) (total-fitness score-population))

;roulette wheel
(defn selection [score-population index slice]
      (cond
        (< slice 0) (- index 1)
        :else (selection (rest score-population) (+ index 1) (- slice (correct-fitness (first score-population))))
        )
      )

;function
(defn selected [score-population]
      (selection score-population INDEX ((fn [fSlice] (rand-int fSlice)) (total-fitness score-population)))
      )
