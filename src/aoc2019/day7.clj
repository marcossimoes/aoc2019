(ns aoc2019.day7
  (:require [clojure.core.async :as async :refer [<!! thread]]
            [clojure.java.io :as io]
            [clojure.tools.trace :refer [trace]])
  (:import (java.io PipedReader PipedWriter)))

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

(def phases (let [rng #{0 1 2 3 4}]
              (for [a rng
                    b (disj rng a)
                    c (disj rng a b)
                    d (disj rng a b c)
                    e (disj rng a b c d)]
                [a b c d e])))

(def instructions [3, 8, 1001, 8, 10, 8, 105, 1, 0, 0, 21, 34, 51, 64, 73, 98, 179, 260, 341, 422, 99999, 3, 9, 102, 4, 9, 9, 1001, 9, 4, 9, 4, 9, 99, 3, 9, 1001, 9, 4, 9, 1002, 9, 3, 9, 1001, 9, 5, 9, 4, 9, 99, 3, 9, 101, 5, 9, 9, 102, 5, 9, 9, 4, 9, 99, 3, 9, 101, 5, 9, 9, 4, 9, 99, 3, 9, 1002, 9, 5, 9, 1001, 9, 3, 9, 102, 2, 9, 9, 101, 5, 9, 9, 1002, 9, 2, 9, 4, 9, 99, 3, 9, 1001, 9, 1, 9, 4, 9, 3, 9, 1002, 9, 2, 9, 4, 9, 3, 9, 1001, 9, 2, 9, 4, 9, 3, 9, 1001, 9, 2, 9, 4, 9, 3, 9, 102, 2, 9, 9, 4, 9, 3, 9, 102, 2, 9, 9, 4, 9, 3, 9, 101, 1, 9, 9, 4, 9, 3, 9, 101, 1, 9, 9, 4, 9, 3, 9, 102, 2, 9, 9, 4, 9, 3, 9, 101, 2, 9, 9, 4, 9, 99, 3, 9, 101, 1, 9, 9, 4, 9, 3, 9, 1001, 9, 1, 9, 4, 9, 3, 9, 102, 2, 9, 9, 4, 9, 3, 9, 102, 2, 9, 9, 4, 9, 3, 9, 101, 2, 9, 9, 4, 9, 3, 9, 101, 1, 9, 9, 4, 9, 3, 9, 101, 1, 9, 9, 4, 9, 3, 9, 1002, 9, 2, 9, 4, 9, 3, 9, 1002, 9, 2, 9, 4, 9, 3, 9, 1001, 9, 2, 9, 4, 9, 99, 3, 9, 101, 2, 9, 9, 4, 9, 3, 9, 102, 2, 9, 9, 4, 9, 3, 9, 1001, 9, 2, 9, 4, 9, 3, 9, 1001, 9, 2, 9, 4, 9, 3, 9, 1001, 9, 2, 9, 4, 9, 3, 9, 101, 2, 9, 9, 4, 9, 3, 9, 1001, 9, 1, 9, 4, 9, 3, 9, 102, 2, 9, 9, 4, 9, 3, 9, 102, 2, 9, 9, 4, 9, 3, 9, 101, 2, 9, 9, 4, 9, 99, 3, 9, 1002, 9, 2, 9, 4, 9, 3, 9, 102, 2, 9, 9, 4, 9, 3, 9, 1001, 9, 2, 9, 4, 9, 3, 9, 1002, 9, 2, 9, 4, 9, 3, 9, 1001, 9, 2, 9, 4, 9, 3, 9, 1002, 9, 2, 9, 4, 9, 3, 9, 102, 2, 9, 9, 4, 9, 3, 9, 102, 2, 9, 9, 4, 9, 3, 9, 101, 2, 9, 9, 4, 9, 3, 9, 1001, 9, 2, 9, 4, 9, 99, 3, 9, 1001, 9, 1, 9, 4, 9, 3, 9, 101, 1, 9, 9, 4, 9, 3, 9, 1002, 9, 2, 9, 4, 9, 3, 9, 1001, 9, 2, 9, 4, 9, 3, 9, 1001, 9, 1, 9, 4, 9, 3, 9, 101, 1, 9, 9, 4, 9, 3, 9, 102, 2, 9, 9, 4, 9, 3, 9, 1001, 9, 2, 9, 4, 9, 3, 9, 1002, 9, 2, 9, 4, 9, 3, 9, 1001, 9, 2, 9, 4, 9, 99])
(def sample-instructions [3, 15, 3, 16, 1002, 16, 10, 16, 1, 16, 15, 15, 4, 15, 99, 0, 0])
(def sample-instructions-2 [3, 23, 3, 24, 1002, 24, 10, 24, 1002, 23, -1, 23, 101, 5, 23, 23, 1, 24, 23, 23, 4, 23, 99, 0, 0])
(def sample-instructions-3 [3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33, 1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0])

(defn setting-output
  [instructions setting input]
  (println (first setting))
  (println input)
  (run instructions)
  (let [phase-output (Integer. (read-line))]
    (if (= 1 (count setting))
      phase-output
      (setting-output instructions (rest setting) phase-output))))

(defn test-setting
  ([instructions setting] (test-setting instructions setting 0))
  ([instructions setting input]
   (with-open [pipedreader (PipedReader.)
               pipedwriter (PipedWriter. pipedreader)]
     (binding [*out* (io/writer pipedwriter)
               *in* (io/reader pipedreader)]
       (setting-output instructions setting input)))))

(defn setting-of-each-output
  [instructions]
  (let [possible-phases #{0 1 2 3 4}]
    (apply (partial merge-with min)
           (for [a possible-phases
                 b (disj possible-phases a)
                 c (disj possible-phases a b)
                 d (disj possible-phases a b c)
                 e (disj possible-phases a b c d)]
             (hash-map (test-setting instructions [a b c d e])
                       (Integer. (str a b c d e)))))))

(defn setting-with-max-thruster
  [instructions]
  (let [setting-of-each-output (setting-of-each-output instructions)]
    (->> setting-of-each-output
         keys
         (apply max)
         (setting-of-each-output))))

(defn max-thruster
  [instructions]
  (let [setting-of-each-output (setting-of-each-output instructions)]
    (->> setting-of-each-output
         keys
         (apply max))))

(and (= 43210 (setting-with-max-thruster sample-instructions))
     (= 43210 (max-thruster sample-instructions))
     (= 1234 (setting-with-max-thruster sample-instructions-2))
     (= 54321 (max-thruster sample-instructions-2))
     (= 10432 (setting-with-max-thruster sample-instructions-3))
     (= 65210 (max-thruster sample-instructions-3)))

(max-thruster instructions)