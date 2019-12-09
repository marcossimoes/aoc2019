(ns aoc2019.day5)

(def program-instructions
  [3,225,1,225,6,6,1100,1,238,225,104,0,1101,40,27,224,101,-67,224,224,4,224,1002,223,8,223,1001,224,2,224,1,224,223,223,1101,33,38,225,1102,84,60,225,1101,65,62,225,1002,36,13,224,1001,224,-494,224,4,224,1002,223,8,223,1001,224,3,224,1,223,224,223,1102,86,5,224,101,-430,224,224,4,224,1002,223,8,223,101,6,224,224,1,223,224,223,1102,23,50,225,1001,44,10,224,101,-72,224,224,4,224,102,8,223,223,101,1,224,224,1,224,223,223,102,47,217,224,1001,224,-2303,224,4,224,102,8,223,223,101,2,224,224,1,223,224,223,1102,71,84,225,101,91,40,224,1001,224,-151,224,4,224,1002,223,8,223,1001,224,5,224,1,223,224,223,1101,87,91,225,1102,71,19,225,1,92,140,224,101,-134,224,224,4,224,1002,223,8,223,101,1,224,224,1,224,223,223,2,170,165,224,1001,224,-1653,224,4,224,1002,223,8,223,101,5,224,224,1,223,224,223,1101,49,32,225,4,223,99,0,0,0,677,0,0,0,0,0,0,0,0,0,0,0,1105,0,99999,1105,227,247,1105,1,99999,1005,227,99999,1005,0,256,1105,1,99999,1106,227,99999,1106,0,265,1105,1,99999,1006,0,99999,1006,227,274,1105,1,99999,1105,1,280,1105,1,99999,1,225,225,225,1101,294,0,0,105,1,0,1105,1,99999,1106,0,300,1105,1,99999,1,225,225,225,1101,314,0,0,106,0,0,1105,1,99999,1107,226,677,224,1002,223,2,223,1006,224,329,101,1,223,223,8,226,226,224,1002,223,2,223,1005,224,344,101,1,223,223,1007,677,226,224,102,2,223,223,1005,224,359,101,1,223,223,8,226,677,224,102,2,223,223,1005,224,374,101,1,223,223,1107,677,677,224,1002,223,2,223,1005,224,389,1001,223,1,223,108,226,677,224,102,2,223,223,1005,224,404,1001,223,1,223,108,677,677,224,1002,223,2,223,1006,224,419,101,1,223,223,107,677,677,224,102,2,223,223,1006,224,434,101,1,223,223,108,226,226,224,1002,223,2,223,1006,224,449,1001,223,1,223,8,677,226,224,1002,223,2,223,1005,224,464,101,1,223,223,1108,226,677,224,1002,223,2,223,1006,224,479,1001,223,1,223,1108,677,677,224,1002,223,2,223,1005,224,494,101,1,223,223,7,677,677,224,1002,223,2,223,1005,224,509,101,1,223,223,1007,677,677,224,1002,223,2,223,1005,224,524,101,1,223,223,7,677,226,224,1002,223,2,223,1005,224,539,101,1,223,223,1107,677,226,224,102,2,223,223,1006,224,554,101,1,223,223,107,226,677,224,1002,223,2,223,1005,224,569,101,1,223,223,107,226,226,224,1002,223,2,223,1005,224,584,101,1,223,223,1108,677,226,224,102,2,223,223,1006,224,599,1001,223,1,223,1008,677,677,224,102,2,223,223,1006,224,614,101,1,223,223,7,226,677,224,102,2,223,223,1005,224,629,101,1,223,223,1008,226,677,224,1002,223,2,223,1006,224,644,101,1,223,223,1007,226,226,224,1002,223,2,223,1005,224,659,1001,223,1,223,1008,226,226,224,102,2,223,223,1006,224,674,1001,223,1,223,4,223,99,226])

(defn param-arg
  [instructions param mode]
  (if (= mode 0)
    (instructions param)
    param))

(defn op-func
  [func instructions pointer [mode1 mode2]]
  (let [param1 (instructions (+ pointer 1))
        param2 (instructions (+ pointer 2))
        param3 (instructions (+ pointer 3))
        arg1 (param-arg instructions param1 mode1)
        arg2 (param-arg instructions param2 mode2)
        res (func arg1 arg2)
        res-pos param3]
    (assoc instructions res-pos res)))

(defn get-user-input-and-save
  [instructions pointer]
  (let [save-pos (instructions (+ pointer 1))
        user-input (Integer. (read-line))]
    (assoc instructions save-pos user-input)))

(defn get-value
  [instructions pointer [mode]]
  (let [param (instructions (+ pointer 1))
        res (param-arg instructions param mode)]
    (println res)
    instructions))

(defn x-power-of-y [x y] (->> (iterate #(* % x) x) (take y) last))

(defn parsed-1st-instruction
  ([opcode] (parsed-1st-instruction opcode [1 1 1 2] []))
  ([to-be-parsed digits-per-mode parsed]
   (if (empty? digits-per-mode)
     parsed
     (recur
       (quot to-be-parsed (x-power-of-y 10 (last digits-per-mode)))
       (drop-last digits-per-mode)
       (conj parsed (rem to-be-parsed (x-power-of-y 10 (last digits-per-mode))))))))

(parsed-1st-instruction 11012)

(defn jump-if
  [bool instructions pointer [mode1 mode2]]
  (let [param1 (instructions (+ pointer 1))
        param2 (instructions (+ pointer 2))
        arg1 (param-arg instructions param1 mode1)
        arg2 (param-arg instructions param2 mode2)]
    (if (bool (zero? arg1))
      (+ pointer 3)
      arg2)))

(defn run
  ([instructions]
   (run instructions 0))
  ([instructions pointer]
   (let [[opcode & modes] (-> pointer instructions parsed-1st-instruction)]
     (case opcode
       1 (run (op-func + instructions pointer modes) (+ pointer 4))
       2 (run (op-func * instructions pointer modes) (+ pointer 4))
       3 (run (get-user-input-and-save instructions pointer) (+ pointer 2))
       4 (run (get-value instructions pointer modes) (+ pointer 2))
       5 (run instructions (jump-if true? instructions pointer modes))
       6 (run instructions (jump-if false? instructions pointer modes))
       7 (run (op-func #(if (< %1 %2) 1 0) instructions pointer modes) (+ pointer 4))
       8 (run (op-func #(if (= %1 %2) 1 0) instructions pointer modes) (+ pointer 4))
       99 instructions
       (println "ERROR!!!")))))

(run program-instructions)