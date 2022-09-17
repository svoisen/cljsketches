(ns cljsketches.circle-packing
  (:require
    [thi.ng.geom.svg.adapter :as adapt]
    [thi.ng.geom.svg.core :as svg]
    [thi.ng.geom.core :as g]
    [thi.ng.geom.utils.intersect :as isec]
    [thi.ng.geom.circle :as c]
    [thi.ng.geom.rect :as r]
    [thi.ng.math.core :as m]))

(def width 600)
(def height 600)
(def max-radius 200)
(def min-radius 10)
(def tile-size 100)
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
  "Return true if the given circle is in the sketch boundaries."
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

(defn is-valid-circle?
  "Returns true if the given circle is in bounds and does not intersect with any 
  other circle."
  [circle all-tiles]
  (if (not (inside-bounds? circle)) false 
    (let [intersecting-tiles (get-intersecting-tiles circle all-tiles)
          candidate-circles (flatten (map #(:circles %) intersecting-tiles))]
      (true? (not-any? #(isec/intersect-circle-circle? % circle) candidate-circles)))))

(defn rand-point 
  "Get a random point in sketch boundaries."
  []
  [(rand width) (rand height)])

(defn add-circle 
  "Add a circle to the sketch, starting with an initial radius and attempting to 
  grow it until it intersects the sketch boundaries or hits another circle."
  [data start-circle]
  (let [all-tiles (get data :tiles)
        all-circles (get data :circles)] 
    (loop [circle start-circle
           prev-circle start-circle]
      (if (not (is-valid-circle? circle all-tiles))
        ; If it's not a valid circle (out of bounds or hits another circle) stop
        ; and add the last valid circle to our data.
        {:tiles (add-circle-to-intersecting-tiles prev-circle all-tiles) 
         :circles (conj all-circles circle)}
        ; If it is a valid circle, increment the radius by 1 and determine if we
        ; can fit a larger circle in the same space.
        (recur (c/circle (:p circle) (+ 1 (:r circle))) circle)))))

(defn generate-circles [num-circles]
  (let [seeds (map #(c/circle % min-radius) (take num-circles (repeatedly rand-point)))
        initial-data {:tiles initial-tiles :circles []}]
    (reduce add-circle initial-data seeds)))

(def sketch
  (svg/svg {:width width :height height}))

(defn -main
  "Run the sketch"
  []
  (->> sketch (adapt/all-as-svg) (svg/serialize) (spit "circle-packing.svg")))
