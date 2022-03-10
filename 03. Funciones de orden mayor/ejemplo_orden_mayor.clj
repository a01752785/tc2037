(ns ejemplo-orden-mayor)

(defn compuesta
  [f g]
  (fn [x] (f (g x))))

(defn f1 [x] (* x x x))

(defn f2 [x] (+ x 2))

(def f3 (compuesta f1 f2))

(def f4 (compuesta f2 f1))