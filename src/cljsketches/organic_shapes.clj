(ns cljsketches.organic-shapes
  (:require 
    [thi.ng.geom.svg.adapter :as adapt]
    [thi.ng.geom.svg.core :as svg]
    [thi.ng.math.core :as m]
    [thi.ng.math.noise :as n]))

(def width 600)
(def height 600)
(def noise-x-mul 2.25)
(def noise-y-mul 2.25)

(defn get-noise [point]
  (n/noise2 (* noise-x-mul (/ (first point) width)) (* noise-y-mul (/ (second point) height))))

(defn blob [radius center resolution]
  (let [make-point (fn [angle] 
                     [(+ (first center) (* (Math/cos angle) radius)) 
                      (+ (second center) (* (Math/sin angle) radius))])
        noisy-point (fn [point] 
                      (let [noise (* 100.0 (get-noise point))] 
                        [(+ noise (first point)) (+ noise (second point))]))
        make-angles (fn [] (map #(* (/ m/TWO_PI resolution) %) (take resolution (range))))
        points (map #(noisy-point (make-point %)) (make-angles))]
    (svg/polygon points)))

(def sketch
  (svg/svg {:width width :height height}
           (blob 100 [(/ width 2) (/ height 2)] 100)))

(defn -main
  "Run the sketch"
  []
  (->> sketch (adapt/all-as-svg) (svg/serialize) (spit "organic-shapes.svg")))
