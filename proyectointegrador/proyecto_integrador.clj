(ns proyecto-integrador)


(defrecord Machine
  [memory pc sp])


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
  [{:keys [pc] :as machine}]
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


(defn st
  "Function that takes a von neumann machine, pops a value from
  the stack and stores it at memory location."
  [{:keys [memory pc sp] :as machine}]
  (assoc machine
         :memory (assoc memory
                        (nth memory (inc pc))
                        (memory sp))
         :pc (+ pc 2)
         :sp (inc sp)))


(defn sti
  "Function that takes a von neumann machine, pops an index from
  the stack, then pops a value from the stack
  and stores the value at the memory location of the index."
  [{:keys [memory pc sp] :as machine}]
  (assoc machine
         :memory (assoc memory
                        (nth memory sp)
                        (memory (inc sp)))
         :pc (inc pc)
         :sp (+ sp 2)))


(defn vnm-pop
  "Function that takes a von neumann machine and discards the
  top of the stack."
  [{:keys [pc sp] :as machine}]
  (assoc machine
         :pc (inc pc)
         :sp (inc sp)))


(defn swp
  "Function that takes a von neumann machine, pops two elements
  from the stack and push them back in reverse order."
  [{:keys [memory pc sp] :as machine}]
  (let [t1 (memory sp)
        t2 (memory (inc sp))]
    (assoc machine
           :memory (assoc memory
                          sp
                          t2
                          (inc sp)
                          t1)
           :pc (inc pc))))


(defn dup
  "Function that takes a von neumann machine, pops a value from
  the stack and push it back twice."
  [{:keys [memory pc sp] :as machine}]
  (assoc machine
         :memory (assoc memory
                        (dec sp)
                        (memory sp))
         :pc (inc pc)
         :sp (dec sp)))


(defn eqz
  "Function that takes a von neumann machine, pops the top
  of the stack and if the value is equal to zero push 1,
  otherwise push 0."
  [{:keys [memory pc sp] :as machine}]
  (assoc machine
         :memory (if (zero? (memory sp))
                   (assoc memory
                          sp
                          1)
                   (assoc memory
                          sp
                          0))
         :pc (inc pc)))


(defn jp
  "Function that takes a von neumann machine and continues
  program execution at the instruction contained at memory
  location index."
  [{:keys [memory pc] :as machine}]
  (assoc machine :pc (memory (inc pc))))


(defn jpc
  "Function that takes a von neumann machine, pops a value from the
  stack and if it’s not equal to zero continues program execution
  at the instruction contained at memory location index,
  otherwise continue with next instruction."
  [{:keys [memory pc sp] :as machine}]
  (assoc machine
         :pc (if (not= (memory sp) 0)
               (memory (inc pc))
               (+ pc 2))
         :sp (inc sp)))


(defn jpi
  "Function that takes a von neumann machine, pops index
  from the stack. Continue program execution at the instruction
  contained at memory location index."
  [{:keys [memory sp] :as machine}]
  (assoc machine
         :pc (memory sp)
         :sp (inc sp)))


(defn out
  "Function that takes a von neumann machine, prints the top
  value of the stack and removes it. Returns a new machine with
  the program counter and the stack pointer increased."
  [{:keys [memory pc sp] :as machine}]
  (print (str (memory sp) " "))
  (assoc machine
         :pc (inc pc)
         :sp (inc sp)))


(defn chr
  "Function that takes a von neumann machine, pops a value
  from the stack, prints to the standard output the character
  with a Unicode code point equal to value. Returns a new machine
  with the program counter and the stack pointer increased."
  [{:keys [memory pc sp] :as machine}]
  (print (char (memory sp)))
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
  {1 nop
   2 ld
   3 ldi
   4 ct
   5 st
   6 sti
   7 vnm-pop
   8 swp
   9 dup
   10 (make-operation +)
   11 (make-operation -)
   12 (make-operation *)
   13 (make-operation quot)
   14 (make-operation rem)
   15 eqz
   16 (make-operation #(if (= %1 %2) 1 0))
   17 (make-operation #(if (not= %1 %2) 1 0))
   18 (make-operation #(if (< %1 %2) 1 0))
   19 (make-operation #(if (<= %1 %2) 1 0))
   20 (make-operation #(if (> %1 %2) 1 0))
   21 (make-operation #(if (>= %1 %2) 1 0))
   22 jp
   23 jpc
   24 jpi
   25 out
   26 chr})


(defn execute
  "Function that takes a machine code in the form of a vector
  and the size of the machine memory and executes it following the
  opcode dictionary."
  [code size]
  (loop [machine (make-machine code size)]
    (let [{:keys [memory pc]} machine
          opcode (memory pc)]
      (if (zero? opcode)
        (print "\nProgram terminated.\n")
        (if (contains? operations opcode)
          (recur ((operations opcode) machine))
          (throw (ex-info (str "Invalid opcode: " opcode) ())))))))


(defn tokenizer
  "Function that takes the name of a file containing a Von
  Neumann Assembly script and converts it to a sequence of symbols
  and numbers corresponding to the file. It ignores comments."
  [file-name]
  (as-> (slurp file-name) here
        (clojure.string/replace here
                                #";.*"
                                "")
        (clojure.string/split here
                              #"\s+")
        (remove #(= % "") here)
        (map clojure.edn/read-string here)))


(def tokens-to-opcodes
  {'hlt 0
   'nop 1
   'ld 2
   'ldi 3
   'ct 4
   'st 5
   'sti 6
   'pop 7
   'swp 8
   'dup 9
   'add 10
   'sub 11
   'mul 12
   'div 13
   'rem 14
   'eqz 15
   'ceq 16
   'cne 17
   'clt 18
   'cle 19
   'cgt 20
   'cge 21
   'jp 22
   'jpc 23
   'jpi 24
   'out 25
   'chr 26})


(defn correct-operands?
  "Function that takes a sequence of assembly tokens and verifies that
  a value exists where it is expected. Returns true if operands are correct,
  throws an exception otherwise."
  [tokens]
  (loop [tokens tokens]
    (if (zero? (count tokens))
      true
      (if (or (= 'ld (first tokens))
              (= 'ct (first tokens))
              (= 'st (first tokens))
              (= 'jp (first tokens))
              (= 'jpc (first tokens)))
        (if (not (contains? tokens-to-opcodes (second tokens)))
          (recur (rest tokens))
          (throw (Exception. (str "Invalid syntax error after: " (first tokens)))))
        (recur (rest tokens))))))


(defn not-redefined-labels?
  "Function that receives a sequence of assembly tokens and verifies that
  there are not redefined labels. Returns true if checks are correct,
  throws an exception otherwise."
  [tokens]
  (loop [tokens tokens
         labels #{}]
    (if (zero? (count tokens))
      true
      (if (= 'label (first tokens))
        (if (contains? labels (second tokens)) ; Already defined label
          (throw (Exception. (str "Redefined label error with: " (second tokens))))
          (recur (rest (rest tokens)) ; Add current label to the labels set
                 (conj labels (second tokens))))
        (recur (rest tokens) ; Not a label
               labels)))))


(defn not-missing-declaration?
  "Function that receives a sequence of assembly tokens and verifies that
  there are not missing declarations of labels. Returns true if checks
  are correct, throws an exception otherwise."
  [tokens]
  (loop [tokens tokens
         used-labels #{}
         declared-labels #{}]
    (if (zero? (count tokens))
      ;; Compute difference between the used labels and the declared labels
      (let [missing-labels (clojure.set/difference used-labels declared-labels)]
        (if (not (empty? missing-labels))
          ;; There are missing declarations
          (throw (Exception. (str "Missing label declaration of:" missing-labels)))
          ;; No missing declarations
          true))
      ;; Check if the next token is an operation needing a value, with a label
      (if (and (or (= 'ld (first tokens))
                   (= 'ct (first tokens))
                   (= 'st (first tokens))
                   (= 'jp (first tokens))
                   (= 'jpc (first tokens)))
               (instance? clojure.lang.Symbol (second tokens)))
        ;; Add current label to used-labels
        (recur (rest tokens)
               (conj used-labels (second tokens))
               declared-labels)
        ;; Check if next token is a label directive
        (if (= 'label (first tokens))
          ;; Add current label to declared-labels
          (recur (rest tokens)
                 used-labels
                 (conj declared-labels (second tokens)))
          ;; Not an operation with value or label directive
          (recur (rest tokens)
                 used-labels
                 declared-labels))))))


(defn correct-syntax?
  "Function that receives a sequence of assembly tokens and
  verifies that its syntax is correct. Returns true if all checks
  are correct. An exception is thrown otherwise."
  [tokens]
  (and (correct-operands? tokens)
       (not-redefined-labels? tokens)
       (not-missing-declaration? tokens)))


(defn replace-labels
  "Function that takes a sequence representing the machine code
  and a dictionary with pairs (label, value) and replaces labels
  found in the code to its corresponding number value. Returns
  a new sequence with the labels replaced."
  [code labels]
  (map #(labels % %) code))


(defn assembling-handler
  "Function that takes a sequence of assembly tokens and returns
  a sequence corresponding to the Von Neumann machine code."
  [tokens]
  (loop [code []
         tokens tokens
         labels {}]
    (if (zero? (count tokens))
      (replace-labels (reverse code) labels)
      (cond
        ;; Add to the labels map a binding pointing to the next free index of memory
        (= 'label (first tokens)) (recur code
                                         (rest (rest tokens))
                                         (assoc labels
                                                (second tokens) (count code)))

        ;; Add to the code a value of data in the next free index of memory
        (= 'data (first tokens)) (recur (cons (second tokens)
                                              code)
                                        (rest (rest tokens))
                                        labels)

        ;; Convert token to opcode and add to the code
        :else (recur (cons (tokens-to-opcodes (first tokens) (first tokens)) code)
                     (rest tokens)
                     labels)))))


(defn assemble
  "Function that receives the name of a file containing
  Von Neumman Assembly code and transforms the code into
  machine code. Returns a sequence with the machine code."
  [file-name]
  (let [tokens (tokenizer file-name)]
    (if (correct-syntax? tokens)
      (assembling-handler tokens)
      nil)))


(execute (assemble "proyectointegrador/ejemplo1.von") 128)
(execute (assemble "proyectointegrador/ejemplo2.von") 128)
(execute (assemble "proyectointegrador/ejemplo3.von") 128)
(execute (assemble "proyectointegrador/ejemplo4.von") 128)
(execute (assemble "proyectointegrador/ejemplo5.von") 128)
