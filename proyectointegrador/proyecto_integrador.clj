(ns proyecto-integrador)

(defrecord Machine [memory pc sp])

(defn make-machine
  [code size]
  (->Machine (vec (take size (concat code
                                     (repeat 0))))
             0
             size))

(defn nop
  [{:keys [memory pc sp] :as machine}]
  (assoc machine :pc (inc pc)))

(defn ld
  [{:keys [memory pc sp] :as machine}]
  (assoc machine
    :memory (assoc memory
              (dec sp)
              (nth memory (inc pc)))
    :pc (+ pc 2)
    :sp (dec sp)))

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
    :sp (inc sp)))

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
   1 nop
   2 ld
   3 nop
   4 ct
   5 nop
   6 nop
   7 nop
   8 nop
   9 nop
   10 (make-operation +)
   11 (make-operation -)
   12 (make-operation *)
   13 (make-operation quot)
   14 (make-operation rem)
   16 (make-operation #(if (= %1 %2) 1 0))
   17 (make-operation #(if (not= %1 %2) 1 0))
   18 (make-operation #(if (< %1 %2) 1 0))
   19 (make-operation #(if (<= %1 %2) 1 0))
   20 (make-operation #(if (> %1 %2) 1 0))
   21 (make-operation #(if (>= %1 %2) 1 0))
   22 nop
   23 nop
   24 nop
   25 out
   26 nop
   })

(defn execute
  [code size]
  (loop [machine (make-machine code size)]
    (let [{:keys [memory pc sp]} machine
          opcode (memory pc)]
      (if (zero? opcode)
        (print "\nProgram terminated.\n")
        (if (contains? operations opcode)
          (recur ((operations opcode) machine))
          (throw (ex-info (str "Invalid opcode: " opcode) ())))))))


(execute [2 2 25] 20)