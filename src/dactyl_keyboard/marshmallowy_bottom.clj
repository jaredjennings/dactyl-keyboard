(ns dactyl-keyboard.marshmallowy-bottom
  (:refer-clojure :exclude [use import])
  (:require [scad-clj.scad :refer :all]
            [scad-clj.model :refer :all]
            [dactyl-keyboard.util :refer :all]
            [dactyl-keyboard.switch-hole :refer :all]
            [dactyl-keyboard.keycaps :refer :all]
            [dactyl-keyboard.placement :refer :all]
            [dactyl-keyboard.layout :refer :all]
            [dactyl-keyboard.connectors :refer :all]
            [dactyl-keyboard.marshmallowy-sides :refer :all]
            [unicode-math.core :refer :all]))


(defn big-marshmallowy-bottom [flatness downness thickness radius]
  (let [fudged (- downness (* radius 3/10)) #_downness
        finger-big-intersection-shape
        (finger-prism distance-below-to-intersect 0)
        thumb-big-intersection-shape
        (thumb-prism thumb-distance-below -5)
        sph1 (with-fn gasket-sphere-fn (sphere radius))
        sph0 (with-fn gasket-sphere-fn (sphere (- radius thickness)))
        for-finger (fn [sh] (for [row rows #_(range (- (first rows) 1)
                                             (+ (last rows) 1))]
                              (for [col columns #_(range (- (first columns) 1)
                                               (+ (last columns) 1))]
                                (if (finger-has-key-place-p row col)
                                  (key-place col row
                                             (translate [0 0 fudged] sh))))))
        for-thumb (fn [sh] (for [row [-1 0 1] #_[-2 -1 0 1 2]]
                             (for [col [0 1 2] #_[-1 0 1 2 3]]
                               (thumb-place col row
                                            (translate [0 0 fudged] sh)))))
        ; this -5 sets how far away from the keys the top of the
                                        ; marshmallowy sides will be.
                                        ; 6 was orig written in silo
                                        ; definition. from there i
                                        ; don't kknow where it came
                                        ; from.
        kp-down (+ 6 radius)
        kp-shrink -20
        extra-finger-silos
        (fn [& coordss]
          (apply hull
                 (for [[column row] coordss]
                   (silo kp-down distance-below-to-intersect kp-shrink
                       key-place key-silo-widenings column row (sa-cap 1)))))
        key-prism (union
                   (thumb-key-prism kp-down thumb-distance-below kp-shrink)
                   (finger-key-prism kp-down distance-below-to-intersect kp-shrink)
                                        ; FUDGE: remove extra bits of top of finger hull grid
                   (extra-finger-silos [2 4] [2 5])
                   (extra-finger-silos [3 4] [3 5]))
        ]
    (union
    (intersection
     (difference
      (union
       (hull-a-grid (for-finger sph1))
       (hull-a-grid (for-thumb sph1)))
      (union
       (hull-a-grid (for-finger sph0))
       (hull-a-grid (for-thumb sph0)))
      key-prism)
     (union
      finger-big-intersection-shape
      thumb-big-intersection-shape)))))

(def mallowy-bottom-right
  (render (big-marshmallowy-bottom marshmallowy-sides-flatness
                                   marshmallowy-sides-downness
                                   marshmallowy-sides-thickness
                                   marshmallowy-sides-radius)))