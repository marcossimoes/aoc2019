(ns aoc2019.day4
  (:require [clojure.tools.trace :refer [trace]]))

(defn str-vals->int [str-list] (map (comp #(Integer. %) str) str-list))

(defn input->int-vecs
  [input-str]
  (->> input-str
       (partition-by #{\-})
       (remove #{'(\-)})
       (map str-vals->int)
       (map vec)
       vec))

(input->int-vecs "156218-652527")

(defn last-index? [vect index] (= (count vect) (inc index)))
(defn last-index [vect] (dec (count vect)))
(defn second-to-last-index [vect] (- (count vect)
                                     2))

(defn non-decreasing-at-index?
  [vect index]
  (and (not (last-index? vect index))
       (> (vect index)
          (vect (inc index)))))

(defn update-all-nxt-indexes-with
  [vect index v]
  (let [nxt-index (inc index)
        updated-nxt-index (assoc vect (inc index) v)]
    (if (last-index? vect nxt-index)
      updated-nxt-index
      (recur updated-nxt-index nxt-index v))))

(defn nxt-non-decreasing
  ([vect] (nxt-non-decreasing vect 0))
  ([vect index]
   (if (last-index? vect index)
     vect
     (if (non-decreasing-at-index? vect index)
       (update-all-nxt-indexes-with vect index (vect index))
       (recur vect (inc index))))))

(defn has-double?
  "Assumes vec is non decreasing"
  [vect]
  (reduce #(or %1 (apply = %2))
          false
          (partition 2 1 vect)))

(reduce #(or %1 (apply = %2)) false (partition 2 1 [1 2 3 4 5 6]))

(and (false? (has-double? [1 2 3 4 5 6]))
     (true? (has-double? [1 2 3 4 6 6]))
     (true? (has-double? [1 2 3 5 5 6]))
     (true? (has-double? [1 2 4 4 5 6])))

(defn nxt-with-double
  "Assumes vec is non decreasing"
  [vect]
  (if (has-double? vect)
    vect
    (assoc vect (second-to-last-index vect) (last vect))))

(and (= [3 3 3 3 3 3] (nxt-non-decreasing [3 2 3 4 5 6]))
     (= [1 4 4 4 4 4] (nxt-non-decreasing [1 4 3 4 5 6]))
     (= [1 2 5 5 5 5] (nxt-non-decreasing [1 2 5 4 5 6]))
     (= [1 2 3 4 5 7] (nxt-non-decreasing [1 2 3 4 5 6])))

(= [1 2 3 4 6 6] (nxt-with-double [1 2 3 4 5 6]))
(= [1 2 3 4 6 6] (nxt-with-double [1 2 3 4 6 6]))

(defn inc-by-1
  ([vect] (inc-by-1 vect (last-index vect)))
  ([vect
    index]
   (if (> 9 (vect index))
     (update vect index inc)
     (inc-by-1 (assoc vect index 0)
               (dec index)))))

(nth (iterate inc-by-1 [0 0 0 0 0 0]) 1000)

(defn nxt
  [vect]
  (-> vect
      inc-by-1
      nxt-non-decreasing
      nxt-with-double))

(defn less-or-equal-to?
  "assumes both vects have the same length"
  ([vect1 vect2] (less-or-equal-to? vect1 vect2 0))
  ([vect1 vect2 index]
   (or
     (> index (last-index vect1))                           ; all index checked, vect1 still less or equal to vect2
     (< (vect1 index) (vect2 index))                        ; if most left digit is smaller, vect1 is less than vect2
     (and (not (> (vect1 index) (vect2 index)))             ; if most left digit is greater, vect1 is more than vect2
          (less-or-equal-to? vect1 vect2 (inc index))))))   ; if equal check nxt digits

(defn combinations
  [[start end]]
  (->> (nxt start)
       (iterate nxt)
       (take-while #(less-or-equal-to? % end))
       count))

(combinations [[1 1 1 1 1 0] [1 1 1 1 1 1]])
(combinations [[1 5 6 2 1 8] [6 5 2 5 2 7]])

(defn run-app
  [input-str]
  (-> input-str
      input->int-vecs
      combinations))

(run-app "156218-652527")