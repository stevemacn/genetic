(ns myapp.genetic)

;(defn cosx [x] (Math/cos x))
;(defn sinx [x] (Math/sin x))

;constant data
(def POPULATION 5)
(def MEMBERS 9)                                             ;number of term and operator combine must be odd number
(def CHECK_POINTS 10)

;random float
(defn randomFloat [x]
  (rand x)
  )

(defn get-term [val, ind]
  ;(apply (nth '(#'cosx) 0) 0)
  ;(# (nth '(#'cosx #'sinx) ind) val)
  (cond
    ;(= 0 ind) val
    (= (contains? (range 0 9) val)) val
    ;(= 0 ind) (randomFloat val)                             ;this will actual print any number
    (= 1 ind) (Math/cos val)
    (= 2 ind) (Math/sin val)
    (= 3 ind) (/ val 10)                                    ;decimal
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

(defn check-fitness-rec [termop xind]
  (if (or (empty? xind) (empty? termop))
    0                                                       ;find a better error message
    (let [size (count termop) first-x (first xind) rest-terms (rest termop)]
      (cond
        (= size 1) (get-term (first termop) first-x)
        (get-operand
          (get-term (first termop) first-x)                 ;first term
          (check-fitness-rec (rest rest-terms) (rest xind)) ;second term
          (first rest-terms)                                ;operand
          )))
    )
  ;if (first | second | termopRest)
  ;get-operand(get-term(a, firstXind) (check-fitness-rec termopRest xindRest)
  )

;convert to TCO
(defn check-fitness [termOps]
  (check-fitness-rec termOps (get-xindex))                  ;loop recur
  )


(defn intitialize []
  ;(create-population)
  ;loop-recur
  ;(map check-fitness newpopulation)
  ;
  )


;TCO loop to create new populations
;calculate member fitness
;cross-over
;mutate

;generator or all at once? we only support one user - so could be saved in memory.
(defn get-data [] "input=[1,2,3,4,5,6,7,8,9,10]; arr=[];
 for (i in input) arr.push(Math.cos(i)); console.log(arr)")

;sample return value for javascript
;we return the randomData we tried to fit
;we return the equation we found that most closely fit the data
;"randomData = [{x:1,y:3},{x:2,y:7},{x:3,y:1}] function equation(x) { return cos(x)+5*x^2 }

