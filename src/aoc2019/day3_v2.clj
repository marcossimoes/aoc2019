(ns aoc2019.day3-v2)

(defn str->int [s] (Integer. s))

(defn right [pt dist] (-> pt (update 0 + dist) (update 2 conj dist)))
(defn left [pt dist] (-> pt (update 0 - dist) (update 2 conj dist)))
(defn up [pt dist] (-> pt (update 1 + dist) (update 2 conj dist)))
(defn down [pt dist] (-> pt (update 1 - dist) (update 2 conj dist)))

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

(defn pt+vec->line [init-pt v]
  (let [[direc dist] v
        final-pt (direc init-pt dist)]
    [init-pt final-pt]))

(defn wires-from-file-path
  [input-file-path]
  (let [input (line-seq (clojure.java.io/reader input-file-path))]
    (map str->coords input)))

(defn draw-wire
  [[first-vec & rem-wire]]
  (reduce (fn [lines v] (-> lines
                            last
                            ;second = final point in last line
                            second
                            (pt+vec->line v)
                            (as-> new-line (conj lines new-line))))
          [(pt+vec->line [0 0 []] first-vec)]
          rem-wire))

(def wire1 (draw-wire (first (wires-from-file-path "resources/day-3-input-test-1"))))
(def wire2 (draw-wire (second (wires-from-file-path "resources/day-3-input-test-1"))))

(defn intersect?
  [[[x11 y11] [x12 y12]]
   [[x21 y21] [x22 y22]]]
  (and (<= (min x11 x12) (max x21 x22))
       (>= (max x11 x12) (min x21 x22))
       (<= (min y11 y12) (max y21 y22))
       (>= (max y11 y12) (min y21 y22))))

(defn abs [v] (max v (- v)))

(defn const-coord
  "assumes coordinates are from either vertical or
  horizontal lines, and that lines were already
  checked for intersection. In most cases, either
  c11 = c12 or c21 = c22. In the exceptional case that
  the lines are parallel and not on this coord,
  gets the value with smallest absolute value
  (closest to the origin)"
  [c11 c12 c21 c22]
  (cond
    (= c11 c12) c11
    (= c21 c22) c21
    (< (abs c21) (abs c22)) c21
    (> (abs c21) (abs c22)) c22))

(defn intersect-cord
  [coord line1 line2]
  (->> line2
       (into line1)
       (map coord)
       (apply const-coord)))

(defn dist-line-start-to-to-pt
  [[[x-start y-start]] [x-pt y-pt]]
  (+ (abs (- x-start
             x-pt))
     (abs (- y-start
             y-pt))))

(defn intersect [line1 line2]
  (when (intersect? line1 line2)
    (let [x (intersect-cord first line1 line2)
          y (intersect-cord second line1 line2)
          acc-steps-line1 (get-in line1 [1 2])
          acc-steps-line2 (get-in line2 [1 2])
          step1-to-intsct (dist-line-start-to-to-pt line1 [x y])
          step2-to-intsct (dist-line-start-to-to-pt line2 [x y])
          acc-steps-line1-at-intsct (assoc acc-steps-line1 (dec (count acc-steps-line1)) step1-to-intsct)
          acc-steps-line2-at-intsct (assoc acc-steps-line2 (dec (count acc-steps-line2)) step2-to-intsct)]
      [x y [acc-steps-line1-at-intsct acc-steps-line2-at-intsct]])))

(defn check-all-wire-line-pairs-for-intersections
  [wire1 wire2]
  (for [line1 wire1
        line2 wire2]
    (intersect line1 line2)))

(defn wire-intersections
  [wire1 wire2]
  (->> (check-all-wire-line-pairs-for-intersections wire1 wire2)
       ;origin must be ignored
       rest
       ;remove checks that returned nil
       (filter some?)))

(defn pt-manhattan-dist
  [pt]
  (->> (pop pt)
       ;; get absolute value
       (map #(max % (- %)))
       (apply +)))

(defn min-of-manhathan-dist-over-intersections
  [input-file-path]
  (let [[wire1 wire2] (map draw-wire (wires-from-file-path input-file-path))
        [first-pt & rem-pts] (wire-intersections wire1 wire2)
        first-pt-dist (pt-manhattan-dist first-pt)]
    (transduce (map pt-manhattan-dist) min first-pt-dist rem-pts)))

(defn combined-steps [[_ _ [steps1 steps2]]]
  (apply + (into steps1 steps2)))

(defn min-combined-steps-to-intersection
  [input-file-path]
  (let [[wire1 wire2] (map draw-wire (wires-from-file-path input-file-path))
        [first-pt & rem-pts] (wire-intersections wire1 wire2)
        _ (println "intersect:" (into [first-pt] rem-pts) "\n")
        first-pt-dist (combined-steps first-pt)]
    (transduce (map combined-steps) min first-pt-dist rem-pts)))

(min-of-manhathan-dist-over-intersections "resources/day-3-input")

(min-combined-steps-to-intersection "resources/day-3-input")