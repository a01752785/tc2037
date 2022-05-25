(ns proyecto-integrador)

(defrecord Machine [memory pc sp])

(defn make-machine
  "Function that creates an instance of the record Machine,
  initializing it with a piece of code and the size of its memory."
  [code size]
  (->Machine (vec (take size (concat code
                                     (repeat 0))))
             0
             size))

(defn nop
  "Function that takes a von neumann machine and performs
  no operation on it. Returns a new machine with the program
  counter increased."
  [{:keys [memory pc sp] :as machine}]
  (assoc machine :pc (inc pc)))

(defn ld
  "Function that takes a von neumann machine and pushes to the stack
  the value inside the index referred by the memory at position program
  counter + 1. Returns a new machine with the program counter increased
  by 2 and de stack pointer reduced by 1."
  [{:keys [memory pc sp] :as machine}]
  (assoc machine
    :memory (assoc memory
              (dec sp)
              (nth memory (nth memory (inc pc))))
    :pc (+ pc 2)
    :sp (dec sp)))

(defn ldi
  "Function that takes a von neumann machine and pushes to the stack
  the value inside the index referred by the top of the stack.
  Returns a new machine with the program counter increased."
  [{:keys [memory pc sp] :as machine}]
  (assoc machine
    :memory (assoc memory
              sp
              (nth memory (nth memory sp)))
    :pc (inc pc)))

(defn ct
  "Function that takes a von neumann machine and pushes a constant
  value to the stack. Returns a new machine with the program
  counter increased by 2 and the stack pointer reduced by 1."
  [{:keys [memory pc sp] :as machine}]
  (assoc machine
    :memory (assoc memory
              (dec sp)
              (memory (inc pc)))
    :pc (+ pc 2)
    :sp (dec sp)))

(defn out
  "Function that takes a von neumann machine, prints the top
  value of the stack and removes it. Returns a new machine with
  the program counter and the stack pointer increased."
  [{:keys [memory pc sp] :as machine}]
  (print (str (memory sp) " "))
  (assoc machine
    :pc (inc pc)
    :sp (inc sp)))

(defn make-operation
  "Function that receives an operation function and returns a new
  function that applies that operation."
  [operation]
  (fn [{:keys [memory pc sp] :as machine}]
    "Function that takes a von neumann machine and pushes the result
    of applying an operation to the two top-most elements currently
    in the stack. Returns a new machine with the program counter
    and the stack pointer increased."
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
   3 ldi
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
   15 nop
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
  "Function that takes a machine code in the form of a vector
  and the size of the machine memory and executes it following the
  opcode dictionary."
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