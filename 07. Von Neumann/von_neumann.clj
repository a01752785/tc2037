(ns von-neumann)

(defrecord Machine [memory pc sp])

(defn make-machine
  [code size]
  (->Machine (vec (take size (concat code
                                     (repeat 0))))
             0
             size))

(make-machine [4 42 25 0] 10)