(ns aoc2019.day6
  (:require [clojure.tools.trace :refer [trace]]))

(defn total-orbits
  ([orbiting-centers] (total-orbits orbiting-centers "COM" 0))
  ([orbiting-centers center orbit-level]
   (if-let [orbiting (orbiting-centers center)]
     (->> orbiting
          (reduce #(+ %1 (total-orbits orbiting-centers %2 (inc orbit-level))) 0)
          (+ orbit-level))
     orbit-level)))

(defn orbit-pairs
  [orbit-str-pairs]
  (map #(clojure.string/split % #"\)") orbit-str-pairs))

(defn orbiting-centers
  [orbit-pairs]
  (reduce (fn [res [center obj]]
            (update res center #(conj % obj)))
          {}
          orbit-pairs))

(defn run
  [orbit-pairs-file-path]
  (let [orbit-str-pairs (line-seq (clojure.java.io/reader orbit-pairs-file-path))]
    (->> orbit-str-pairs
         orbit-pairs
         orbiting-centers
         total-orbits)))

(= 273985 (run "resources/day-5-input"))
(= 16 (run "resources/day-5-test-input"))

(defn direct-center-of
  [obj orbiting-centers]
  (some (fn [[center orbiting]]
          (some #(when (= % obj) center) orbiting))
        orbiting-centers))

(defn point-of-interception
  [transfs1 transfs2]
  (let [set1 (set transfs1)
        set2 (set transfs2)]
    (first (clojure.set/intersection set1 set2))))

(defn count-transfs-to-point
  [point transfs1 transfs2]
  (+ (count (take-while #(not= % point) transfs1))
     (count (take-while #(not= % point) transfs2))))

(defn transfs-between
  ([obj1 obj2 orbiting-centers] (transfs-between obj1 obj2 orbiting-centers [] []))
  ([obj1 obj2 orbiting-centers prev-transfs1 prev-transfs2]
   (let [center1 (direct-center-of obj1 orbiting-centers)
         center2 (direct-center-of obj2 orbiting-centers)
         transfs1 (cond-> prev-transfs1 center1 (conj center1))
         transfs2 (cond-> prev-transfs2 center2 (conj center2))
         point (point-of-interception transfs1 transfs2)]
     (cond
       (and (= transfs1 prev-transfs1)
            (= transfs2 prev-transfs2)) nil
       (some? point) (count-transfs-to-point point transfs1 transfs2)
       :else (recur center1 center2 orbiting-centers transfs1 transfs2)))))

(defn run2
  [orbit-pairs-file-path]
  (let [orbit-str-pairs (line-seq (clojure.java.io/reader orbit-pairs-file-path))]
    (->> orbit-str-pairs
         orbit-pairs
         orbiting-centers
         (transfs-between "YOU" "SAN"))))

(= 460 (run2 "resources/day-5-input"))