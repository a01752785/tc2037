(ns parallelism)

(defn bits
  [x]
  (.bitCount (biginteger x)))

(defn fact-sec
  [n]
  (bits
    (loop [i 1
         r 1]
    (if (> i n)
      r
      (recur (inc i)
             (*' i r))))))

(fact-sec 5)