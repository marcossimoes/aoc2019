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
      (recur updated-nxt-index nxt-index v))))              ;otherwise check nxt digit

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

(defn inc-by
  ([vect amount] (inc-by vect amount (last-index vect)))
  ([vect amount index]
   (if (zero? amount)
     vect
     (let [updated-val (+ amount (vect index))
           [val-for-index val-for-nxt-index] ((juxt rem quot) updated-val 10)]
       (inc-by (assoc vect index val-for-index) val-for-nxt-index (dec index))))))

(defn inc-by-1 [vect] (inc-by vect 1))

(defn nxt-with-double
  "Assumes vec is non decreasing"
  [vect]
  (cond
    (has-double? vect) vect
    :else (assoc vect (second-to-last-index vect) (last vect))))

(defn nxt [vect]
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
          (less-or-equal-to? vect1 vect2 (inc index))))))

(defn combinations
  [[start end]]
  (->> (nxt start)
       (iterate nxt)
       (take-while #(less-or-equal-to? % end))
       count))

(defn run-app
  [input-str]
  (-> input-str
      input->int-vecs
      combinations))

(run-app "156218-652527")

;; ### Part II

(defn decreasing?
  ([vect] (decreasing? vect 0))
  ([vect index]
   (and (not (last-index? vect index))                      ;if all indexes were checked, return false
        (or (> (vect index) (vect (inc index)))             ;if current digit is larger than nxt, return true
            (decreasing? vect (inc index))))))

(defn has-exactly-double?
  "Assumes vec is non decreasing"
  [vect]
  (->> vect
       (partition-by identity)
       (some #(= 2 (count %)))))              ; if equal check nxt digits

(defn trio-of-9s->001 [vect] (inc-by vect 2))
(defn xyz->xzz [vect] (assoc vect (second-to-last-index vect) (last vect)))
(defn xxx->xxy "assumes x < 9" [vect] (inc-by vect 1))
(defn xxxx->yyzz [vect] (inc-by vect 11))

(defn nxt-with-exactly-double
  "Assumes vec is non decreasing"
  [vect]
  (let [last-repetition (->> vect (partition-by identity) last)]
    (cond
      (= '(9 9 9) last-repetition) (trio-of-9s->001 vect)
      (= 3 (count last-repetition)) (xxx->xxy vect)
      (< 3 (count last-repetition)) (xxxx->yyzz vect)
      :else (xyz->xzz vect))))

(defn nxt-valid-with-exact-doubles
  [vect]
  (cond
    (decreasing? vect) (nxt-valid-with-exact-doubles (nxt-non-decreasing vect))
    (not (has-exactly-double? vect)) (nxt-valid-with-exact-doubles (nxt-with-exactly-double vect))
    :else vect))

(defn nxt-with-exact-doubles [vect]
  (-> vect
      inc-by-1
      nxt-valid-with-exact-doubles))

(defn combinations-with-exact-doubles
  [[start end]]
  (->> (nxt-with-exact-doubles start)
       (iterate nxt-with-exactly-double)
       (take-while #(less-or-equal-to? % end))
       count))

(defn run-app-with-exact-doubles
  [input-str]
  (-> input-str
      input->int-vecs
      combinations-with-exact-doubles))

(run-app-with-exact-doubles "156218-652527")