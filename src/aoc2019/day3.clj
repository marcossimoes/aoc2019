(ns aoc2019.day3)

(defn str->int [s] (Integer. s))

(defn right [pt] (update pt 0 inc))
(defn left [pt] (update pt 0 dec))
(defn up [pt] (update pt 1 inc))
(defn down [pt] (update pt 1 dec))

(defn char-seq->coord
  "converts a sequence of characters with c d d d format (any number of digits allowed)
  into a coordenate with the following format c ddd where ddd is an integer with n d digits.
  Example the R 1 2 3 seq, becomes R 123"
  [[letter & digits]]
  (let [dist (apply (comp str->int str) digits)
        direc-from ({\R right \L left \U up \D down} letter)]
    [direc-from dist]))

(defn str->coords
  [s]
  (transduce (comp (partition-by #{\,})
                   (remove #{[\,]})
                   (map char-seq->coord))
             conj [] s))

(defn mark-at
  [new-rcd new-pt steps _]
  (update-in new-rcd new-pt conj steps))

(defn report-if-intersects
  [new-rcd new-pt current-steps past-rcd]
  (if-let [all-past-steps (get-in past-rcd new-pt)]
    (conj new-rcd [new-pt [(apply min all-past-steps) current-steps]])
    new-rcd))

(defn move
  [direc-from state dist write-down]
  (if (zero? dist)
    state
    (let [[new-rcd init-pt steps past-rcd] state
          new-pt (direc-from init-pt)
          new-rcd (write-down new-rcd new-pt steps past-rcd)
          new-state [new-rcd new-pt (inc steps) past-rcd]]
      (recur direc-from new-state (dec dist) write-down))))

(defn run-path
  [wire state report]
  (let [[first-segment & rest-wire] wire
        [direc-from dist] first-segment
        new-state (move direc-from state dist report)
        [new-rcd] new-state]
    (if (empty? rest-wire)
      new-rcd
      (recur rest-wire new-state report))))

(defn marked-path
  [wire]
  (let [pt [0 0]
        rcd {}
        steps 1
        state [rcd pt steps {}]
        report mark-at]
    (run-path wire state report)))

(defn intersections-of-wire-with-path
  [wire path]
  (let [pt [0 0]
        new-rcd []
        steps 1
        state [new-rcd pt steps path]
        report report-if-intersects]
    (run-path wire state report)))

(defn wires-from-file-path
  [input-file-path]
  (let [input (line-seq (clojure.java.io/reader input-file-path))
        wire-1 (str->coords (first input))
        wire-2 (str->coords (second input))]
    [wire-1 wire-2]))

(defn intersections
  [input-file-path]
  (let [[wire-1 wire-2] (wires-from-file-path input-file-path)]
    (intersections-of-wire-with-path wire-2 (marked-path wire-1))))

(defn min-of-operation-over-intersections
  [operation input-file-path]
  (let [[first-pt-with-steps & rem-pts-with-steps] (intersections input-file-path)
        first-pt-dist (operation first-pt-with-steps)]
    (transduce (map operation) min first-pt-dist rem-pts-with-steps)))

(defn pt-manhattan-dist [pt]
  (->> (first pt)
       ;; get absolute value
       (map #(max % (- %)))
       (apply +)))

(defn min-common-manhattan-dst
  [input-file-path]
  (min-of-operation-over-intersections pt-manhattan-dist input-file-path))

(= 3247 (min-common-manhattan-dst "resources/day-3-input"))

(defn combined-steps [[_ steps]] (apply + steps))

(defn min-combined-steps-to-intersection
  [input-file-path]
  (min-of-operation-over-intersections combined-steps input-file-path))

(= 30 (min-combined-steps-to-intersection "resources/day-3-input-test-1"))
(= 610 (min-combined-steps-to-intersection "resources/day-3-input-test-2"))
(= 410 (min-combined-steps-to-intersection "resources/day-3-input-test-3"))

(min-combined-steps-to-intersection "resources/day-3-input")