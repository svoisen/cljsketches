(ns cljsketches.circle-packing
  (:require
    [thi.ng.geom.svg.adapter :as adapt]
    [thi.ng.geom.svg.core :as svg]
    [thi.ng.geom.core :as g]
    [thi.ng.geom.vector :as v]
    [thi.ng.geom.utils.intersect :as isec]
    [thi.ng.geom.circle :as c]
    [thi.ng.geom.rect :as r]))

(def width 500)
(def height 500)
(def max-radius 100)
(def min-radius 2)
(def tile-size 100)
(def padding 5)
(def num-attempts 8000)
(def min-container-radius 20)
(def initial-tiles 
  (let [num-cols (/ width tile-size)
        num-rows (/ height tile-size)]
    (vec (for [i (range num-cols)
               j (range num-rows)
               :let [idx (+ j (* i num-cols))]]
           {:idx idx
            :x i
            :y j
            :rect (r/rect (* i tile-size) (* j tile-size) tile-size)
            :circles []}))))

(defn get-intersecting-tiles 
  "Get the tiles that intersect with the given circle."
  [circle tiles]
  (filter #(isec/intersect-rect-circle? (:rect %) circle) tiles))

(defn add-circle-to-tile 
  "Add a circle to the vector of all tiles associating it with the tile at the given index."
  [circle all-tiles tile-idx]
  (let [tile (nth all-tiles tile-idx)
        new-tile (assoc tile :circles (conj (:circles tile) circle))]
    (assoc all-tiles tile-idx new-tile)))

(defn add-circle-to-intersecting-tiles
  "Given a circle, add it to the vector of tiles for each tile that it intersects." 
  [circle all-tiles]
  (let [indices (map #(:idx %) (get-intersecting-tiles circle all-tiles))
        add (partial add-circle-to-tile circle)]
    (reduce add all-tiles indices)))

(defn inside-bounds? 
  "Return true if the given circle is inside the sketch boundaries."
  [circle]
  (let [p (:p circle)
        r (:r circle)
        x (first p)
        y (second p)]
    (if (and 
          (>= (- x r) 0) 
          (<= (+ x r) width)
          (>= (- y r) 0)
          (<= (+ y r) height))
      true false)))

(defn circle-distance 
  "Returns the distance between two circles c0 and c1."
  [c0 c1]
  (- (g/dist (v/vec2 (:p c0)) (v/vec2 (:p c1))) (+ (:r c0) (:r c1))))

(defn is-contained-by?
  "True if c0 is contained within c1 including any padding value."
  [c0 c1]
  (<= (+ (:r c0) (g/dist (v/vec2 (:p c0)) (v/vec2 (:p c1)))) (- (:r c1) padding)))

(defn is-invalid-circle?
  [circle all-tiles]
  (if (or (not (inside-bounds? circle)) (>= (:r circle) max-radius)) true 
    (let [intersecting-tiles (get-intersecting-tiles circle all-tiles)
          candidate-circles (flatten (map #(:circles %) intersecting-tiles))
          any? (complement not-any?)]
      ; TODO This could use some readability improvements.
      ; A circle is invalid if it is not wholly contained inside a larger circle with
      ; a radius bigger than min-container-radius or if its distance to another
      ; circle is less than the padding value.
      (true? (any? #(and 
                      (not (and (>= (:r %) min-container-radius) (is-contained-by? circle %))) 
                      (<= (circle-distance % circle) padding)) candidate-circles)))))

(defn rand-point 
  "Get a random point in sketch boundaries."
  []
  [(+ min-radius (rand (- width min-radius))) (+ min-radius (rand (- height min-radius)))])

(defn grow-circle 
  "Add a circle to the sketch, starting with an initial radius and attempting to 
  grow it until it intersects the sketch boundaries or hits another circle."
  [data start-circle]
  (let [all-tiles (get data :tiles)
        all-circles (get data :circles)] 
    (loop [circle start-circle
           prev-circle nil]
      (if (is-invalid-circle? circle all-tiles)
        ; If it's not a valid circle (out of bounds or hits another circle) stop
        ; and add the last valid circle to our data.
        (if (nil? prev-circle) data 
          {:tiles (add-circle-to-intersecting-tiles prev-circle all-tiles) 
           :circles (conj all-circles circle)})
        ; If it is a valid circle, increment the radius by 1 and determine if we
        ; can fit a larger circle in the same space.
        (recur (c/circle (:p circle) (+ 1 (:r circle))) circle)))))

(defn get-nesting-level 
  "Given a circle, get its level of nesting inside other circles."
  [circle all-circles]
    (count (filter #(is-contained-by? circle %) all-circles)))

(defn get-color 
  "Get a circle's fill color based on its nesting level."
  [circle all-circles]
  (if (even? (get-nesting-level circle all-circles)) "black" (if (> (rand) 0.8) "red" "white")))

(defn grow-circles [num-circles]
  (let [seeds (map #(c/circle % min-radius) (take num-circles (repeatedly rand-point)))
        initial-data {:tiles initial-tiles :circles []}]
    (reduce grow-circle initial-data seeds)))

(def sketch
  (let [circle-data (grow-circles num-attempts)
        colorized-circles (map #(svg/circle (:p %) 
                                            (:r %) 
                                            {:fill (get-color % (:circles circle-data))}) 
                               (:circles circle-data))]
    (svg/svg {:width width :height height} 
             (svg/rect [0 0] width height {:fill "white"})
             colorized-circles)))

(defn -main
  "Run the sketch"
  []
  (->> sketch (adapt/all-as-svg) (svg/serialize) (spit "circle-packing.svg")))
