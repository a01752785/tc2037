(ns ejemplo-repeticiones)

(defn !-1
  [n]
  (if (zero? n)
    1
    (*' n (!-1 (dec n)))))

(!-1 5)
(!-1 10)
(!-1 20)
(!-1 21)
(!-1 1000)

(defn !-2
  [n]
  (loop [result 1
         i 1]
    (if (> i n)
      result
      (recur (*' result i)
             (inc i)
      )
    )
  )
)

(!-2 0)
(!-2 1000)
(!-2 5000)
(!-2 10000)

(defn !-3
  "docstring"
  [n]
  (reduce *' (range 1 (inc n)) ))

(!-3 5)
(!-3 0)
(!-3 10000)