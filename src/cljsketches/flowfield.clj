(ns cljsketches.flowfield
  (:require 
    [thi.ng.geom.svg.adapter :as adapt]
    [thi.ng.geom.svg.core :as svg]
    [thi.ng.math.core :as m]
    [thi.ng.math.noise :as n]))

(def width 600.0)
(def height 600.0)
(def length 15.0)
(def padding 20.0)
(def noise-x-mul 1.125)
(def noise-y-mul 1.25)
(def grey "#999")

(def arrow 
  (svg/arrow-head 5 0.25 true {:fill grey}))

(defn get-noise [x y]
  (n/noise2 (* noise-x-mul (/ x width)) (* noise-y-mul (/ y height))))

(defn noisy-points [res]
  (for [i (range (/ (- width padding) res))
        j (range (/ (- height padding) res)) 
        :let [x (+ (* res i) padding) 
              y (+ (* res j) padding)]]
    {:x x :y y :val (get-noise x y)})
  )

(defn flow-line [{:keys [x y val]}]
  (let [angle (m/map-interval val 0 1.0 0 m/TWO_PI)
        half-len (/ length 2)
        x0 (+ x (* half-len (Math/cos angle)))
        y0 (+ y (* half-len (Math/sin angle)))
        x1 (- x (* half-len (Math/cos angle)))
        y1 (- y (* half-len (Math/sin angle)))]
    (svg/line-strip-decorated 
      [[x0 y0] [x1 y1]] 
      arrow nil nil 
      {:stroke grey}
      )
    )
  )

(defn line [points]
  (svg/line-strip points 
                  {:stroke "red" 
                   :stroke-width 2}))

(defn trace [{:keys [x y points max-points]}]
  (if (or (> x width) (< x 0) (> y height) (< y 0) (> (count points) max-points))
    points
    (let [noise (get-noise x y)
          angle (m/map-interval noise 0 1.0 0 m/TWO_PI)
          new-x (+ x (* 1 (Math/cos angle)))
          new-y (+ y (* 1 (Math/sin angle)))]
      (trace {:x new-x :y new-y :max-points max-points :points (conj points [new-x new-y])}))))

(defn traces [count]
  (let [points (take count (repeatedly (fn [] [(rand width) (rand height)])))]
    (map (fn [point] (trace {:x (get point 0) :y (get point 1) :max-points (rand-int 50) :points ()})) points)))

(def scene
  (let [points (noisy-points 20)]
    (svg/svg {:width width :height height}
             (svg/rect [0 0] width height {:fill "white"})
             (map flow-line points)
             (map #(line %) (filter #(> (count %) 1) (traces 2000))))))

(defn -main
  "Run the sketch"
  []
  (->> scene (adapt/all-as-svg) (svg/serialize) (spit "flowfield.svg")))
