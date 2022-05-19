(ns proyecto-integrador)

(defrecord Machine [memory pc sp])

(defn make-machine
  [code size]
  (->Machine (vec (take size (concat code
                                     (repeat 0))))
             0
             size))

(defn ct
  [{:keys [memory pc sp] :as machine}]
  (assoc machine
    :memory (assoc memory
              (dec sp)
              (memory (inc pc)))
    :pc (+ pc 2)
    :sp (dec sp)))

(defn out
  [{:keys [memory pc sp] :as machine}]
  (print (str (memory sp) " "))
  (assoc machine
           :pc (inc pc)
           :sp (int sp)))

(defn make-operation
  [operation]
  (fn [{:keys [memory pc sp] :as machine}]
    (assoc machine
      :memory (assoc memory
                (inc sp)
                (operation (memory (inc sp))
                   (memory sp)))
      :pc (inc pc)
      :sp (inc sp))))

(def operations
  {
   4 ct
   10 (make-operation +)
   11 (make-operation -)
   12 (make-operation *)
   13 (make-operation quot)
   14 (make-operation rem)
   25 out
   })

(defn execute
  [code size]
  (loop [machine (make-machine code size)]
    (let [{:keys [memory pc sp]} machine
          opcode (memory pc)]
      (if (zero? opcode)
        (println "\nProgram terminated.")
        (if (contains? operations opcode)
          (recur ((operations opcode) machine))
          (throw (ex-info (str "Invalid opcode: " opcode) {})))))))


; Ejemplo de modificar la maquina

;(def m (make-machine [0] 10))

;(assoc m :pc 5
;         :sp 4
;         :memory (assoc (m :memory) 0 5))

(execute [4 42 25 0] 10)

(execute [4 2 4 7 10 25 0] 10)

(execute [4 2 4 7 11 25 0] 10)