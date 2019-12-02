(ns aoc2019.day2)

(def sample-input-1
  [1, 9, 10, 3,
   2, 3, 11, 0,
   99,
   30, 40, 50])

(def sample-output-1
  [3500, 9, 10, 70,
   2, 3, 11, 0,
   99,
   30, 40, 50])

(def sample-input-2 [1, 0, 0, 0, 99])
(def sample-output-2 [2, 0, 0, 0, 99])

(def sample-input-3 [2, 3, 0, 3, 99])
(def sample-output-3 [2, 3, 0, 6, 99])

(def sample-input-4 [2, 4, 4, 5, 99, 0])
(def sample-output-4 [2, 4, 4, 5, 99, 9801])

(def sample-input-5 [1, 1, 1, 4, 99, 5, 6, 0, 99])
(def sample-output-5 [30, 1, 1, 4, 2, 5, 6, 0, 99])


(def program-input
  [1, 0, 0, 3, 1, 1, 2, 3, 1, 3, 4, 3, 1, 5, 0, 3, 2, 6, 1, 19, 1, 19, 5, 23, 2, 9, 23, 27, 1, 5, 27, 31, 1, 5, 31, 35, 1, 35, 13, 39, 1, 39, 9, 43, 1, 5, 43, 47, 1, 47, 6, 51, 1, 51, 13, 55, 1, 55, 9, 59, 1, 59, 13, 63, 2, 63, 13, 67, 1, 67, 10, 71, 1, 71, 6, 75, 2, 10, 75, 79, 2, 10, 79, 83, 1, 5, 83, 87, 2, 6, 87, 91, 1, 91, 6, 95, 1, 95, 13, 99, 2, 99, 13, 103, 1, 103, 9, 107, 1, 10, 107, 111, 2, 111, 13, 115, 1, 10, 115, 119, 1, 10, 119, 123, 2, 13, 123, 127, 2, 6, 127, 131, 1, 13, 131, 135, 1, 135, 2, 139, 1, 139, 6, 0, 99, 2, 0, 14, 0])

(def adjusted-program-input
  [1, 12, 2, 3, 1, 1, 2, 3, 1, 3, 4, 3, 1, 5, 0, 3, 2, 6, 1, 19, 1, 19, 5, 23, 2, 9, 23, 27, 1, 5, 27, 31, 1, 5, 31, 35, 1, 35, 13, 39, 1, 39, 9, 43, 1, 5, 43, 47, 1, 47, 6, 51, 1, 51, 13, 55, 1, 55, 9, 59, 1, 59, 13, 63, 2, 63, 13, 67, 1, 67, 10, 71, 1, 71, 6, 75, 2, 10, 75, 79, 2, 10, 79, 83, 1, 5, 83, 87, 2, 6, 87, 91, 1, 91, 6, 95, 1, 95, 13, 99, 2, 99, 13, 103, 1, 103, 9, 107, 1, 10, 107, 111, 2, 111, 13, 115, 1, 10, 115, 119, 1, 10, 119, 123, 2, 13, 123, 127, 2, 6, 127, 131, 1, 13, 131, 135, 1, 135, 2, 139, 1, 139, 6, 0, 99, 2, 0, 14, 0])

(defn input-processed-by-op
  [input op instruction-pointer]
  (let [param-1 (get input (+ instruction-pointer 1))
        param-2 (get input (+ instruction-pointer 2))
        param-3 (get input (+ instruction-pointer 3))
        arg-1 (get input param-1)
        arg-2 (get input param-2)
        res (op arg-1 arg-2)
        res-pos param-3]
    (assoc input res-pos res)))

(defn run
  ([input]
   (run input 0))
  ([input instruction-pointer]
   (let [opcode (get input instruction-pointer)]
     (case opcode
       1 (run (input-processed-by-op input + instruction-pointer) (+ instruction-pointer 4))
       2 (run (input-processed-by-op input * instruction-pointer) (+ instruction-pointer 4))
       99 input
       (println "ERROR!!!")))))

(defn run-with-adjusts
  [input noun verb]
  (-> input
      (assoc 1 noun)
      (assoc 2 verb)
      run))

(= (run sample-input-1) sample-output-1)
(= (run sample-input-2) sample-output-2)
(= (run sample-input-3) sample-output-3)
(= (run sample-input-4) sample-output-4)
(= (run sample-input-5) sample-output-5)

(run-with-adjusts program-input 12 2)

(defn noun-and-verb-yield
  [noun verb input]
  (-> input
      (assoc 1 noun)
      (assoc 2 verb)
      run
      (get 0)))

(defn noun-and-verb-calc-if-they-yield-res
  [input res [noun verb]]
  (let [noun-and-verb-yields (noun-and-verb-yield noun verb input)]
    (when (= res noun-and-verb-yields)
      (-> noun (* 100) (+ verb)))))

(def noun-and-verb-range
  (for [noun (range 0 100)
        verb (range 0 100)]
    [noun verb]))

(defn find-res
  [input res]
  (some #(noun-and-verb-calc-if-they-yield-res input res %)
        noun-and-verb-range))

(find-res adjusted-program-input 19690720)