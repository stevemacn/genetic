(ns myapp.genetic)

;list of possible terms
;list of possible operators

;initialize (generate random points, generate random population)

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

