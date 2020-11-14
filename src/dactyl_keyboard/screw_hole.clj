;; --- BEGIN AGPLv3-preamble ---
;; Dactyl Marshmallow ergonomic keyboard generator
;; Copyright (C) 2015, 2018 Matthew Adereth and Jared Jennings
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.
;;
;; You should have received a copy of the GNU Affero General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;; --- END AGPLv3-preamble ---
(ns dactyl-keyboard.screw-hole
  (:refer-clojure :exclude [use import])
  (:require [scad-clj.scad :refer :all]
            [scad-clj.model :refer :all]
            [dactyl-keyboard.util :refer :all]
            [dactyl-keyboard.switch-hole :refer [web-thickness]]
            [dactyl-keyboard.shph :refer [screw-hole-pillar-height]]
            [unicode-math.core :refer :all]))

;; Heat-Set Inserts for Plastics
;; https://www.mcmaster.com/#94180A331
;; https://www.matterhackers.com/articles/fasteners-for-3d-printing

(def insert-hole-bottom-diameter 5.1)
(def insert-hole-top-diameter 5.31)
(def insert-height 3.8)
;; #31
(def insert-hole-depth (+ insert-height 2.0))

;; thicker, and you impinge on the switch holes... with the α and β I
;; used anyway.  but too thin and you start squishing the walls when
;; you melt the insert in
(def screw-hole-wall-thickness 1.8)

;; M3 screws.
(def screw-diameter 3)
(def screw-radius (* 1/2 screw-diameter))

;; I can't find my caliper just now; this is a guess.
(def screw-head-diameter 6)
(def screw-head-radius (* 1/2 screw-head-diameter))

;; How far down to set the bottom of the screw head. This factors in
;; the height of the screw head, plus the fact that most of the screw
;; holes are on angled parts of the web, and so must be sunk farther.
(def screw-inset-depth 3)

(def screw-inset-wall-thickness 1)
(def extra-radius-around-screw-head 0.2)
(def screw-inset-outer-radius (+ screw-head-radius
                                 screw-inset-wall-thickness
                                 extra-radius-around-screw-head))

;; this would have been (+ plate-thickness insert-height) but
;; they don't sell custom screws, so i did the math and figured a
;; standard size.
;;
;; https://www.mcmaster.com/#90116A153
(def frame-screw-length 8)

;; frame-screw-hole-minus is moved below, so it can difference out the
;; screw hole pillar below the frame screw hole

;; Owing to the extra height, this will stick up above the web, and
;; the caller will have to difference off the region just above the
;; top surface of the web.
(def frame-screw-hole-plus
  (let [height-factor 4]
    (with-fn 12
      (difference
       (translate [0 0 (- (* ½ height-factor screw-inset-depth)
                          screw-inset-wall-thickness)]
                  (cylinder screw-inset-outer-radius
                            (+ (* 2 screw-inset-wall-thickness)
                               (* height-factor screw-inset-depth))))
       (translate [0 0 (* 2 screw-inset-depth)]
                  (cylinder (- screw-inset-outer-radius
                               screw-inset-wall-thickness)
                            (* 4 screw-inset-depth)))
       (cylinder screw-radius (* 5 web-thickness))))))

;; leave room for diodes and wires; too tall and the teensy runs into
;; the bottom
(def teensy-screw-hole-height (+ insert-hole-depth 1))

;; top is at z=0
(def insert-boss
  (let [bottom-radius (* 1/2 insert-hole-bottom-diameter)
        top-radius (* 1/2 insert-hole-top-diameter)]
    (translate [0 0 (* -1/2 insert-hole-depth)]
               (cylinder [bottom-radius top-radius] insert-hole-depth))))

(def screw-hole-base-diameter 25)
(defn screw-hole-pillar-helper [height bigger-by]
  (let [thic (+ screw-hole-wall-thickness bigger-by)
        top-r (+ (* 1/2 insert-hole-top-diameter) thic)
        bottom-r (+ (* 1/2 screw-hole-base-diameter) bigger-by)
        power 2 ; determines how sharply the horn curves. should be even.
        a (/ (- bottom-r top-r) (Math/pow height power))
        f (fn [y] (+ top-r (* a (Math/pow (- y height) power))))
        steps 5
        step-size (/ height steps)
        horn
        (apply union
               (for [y (range 0 height step-size)]
                 (let [ny (+ y step-size)
                       y-r (f y)
                       ny-r (f ny)]
                   (->> (with-fn 12 (cylinder [y-r ny-r] step-size))
                        (translate [0 0 (- (+ y (* 1/2 step-size))
                                           height)])))))]
    horn))

(defn screw-hole-pillar-plus [height]
  (screw-hole-pillar-helper height 0))
(defn screw-hole-pillar-clearance [height]
  (screw-hole-pillar-helper height 0.5))
(defn screw-hole-pillar-minus [height]
  (->> insert-boss
       (color [1 0 0])
       (translate [0 0 ε])))


;; The screw-hole-minus is much taller than web-thickness, and the
;; plus is taller than it needs to be to contain the screw head,
;; because we are placing screw holes in steeply angled portions of
;; the web.
(def frame-screw-hole-minus
  (with-fn 12
    (union
     (cylinder screw-inset-outer-radius (* 5 web-thickness))
     (translate [0 0 (* 2 screw-inset-depth)]
                (cylinder (- screw-inset-outer-radius
                             screw-inset-wall-thickness)
                          (* 4 screw-inset-depth)))
     (screw-hole-pillar-clearance screw-hole-pillar-height))))


(defn screw-hole-pillar-base [height]
  (translate [0 0 (- height)]
             (with-fn 12
               (cylinder (* 1/2 screw-hole-base-diameter) ε))))

(defn screw-hole-pillar [height]
  (difference (screw-hole-pillar-plus height)
              (screw-hole-pillar-minus height)))

(def screw-hole-pillar-glue-tolerance 0.2)

(defn screw-hole-pillar-splitter [height]
  (let [split-at-height-fraction 3/16
        tz (* split-at-height-fraction height)
        shbd (* 1.5 screw-hole-base-diameter) ;; fudge
        shbr (* 1/2 shbd)
        sample-size (* 1/20 height)
        sample-vector [sample-size sample-size sample-size]
        frequencies [1 1/10 1/7]
        amplitude (* 1/10 height)
        eggcrate (call-module "x_single_eggcrate_box"
                              [(* 1.1 height) ;; fudge
                               shbd shbd]
                              sample-vector
                              frequencies
                              amplitude)]

    (->> eggcrate
         (rotate (* 1/4 τ) [0 1 0])
         (translate [(- shbr) (- shbr) (+ tz (* 1/2 amplitude))]))))

(defn screw-hole-pillar-upper [height]
  (intersection (screw-hole-pillar height)
              (translate [0 0 (+ (- height)
                                 screw-hole-pillar-glue-tolerance)]
                         (screw-hole-pillar-splitter height))))

(defn screw-hole-pillar-lower [height]
  (difference (screw-hole-pillar height)
                (translate [0 0 (- height)]
                           (screw-hole-pillar-splitter height))))

(def screw-hole-for-teensy
  (let [height teensy-screw-hole-height]
    (difference
     (translate [0 0 (* -1/2 height)]
                (cylinder (+ (* 1/2 insert-hole-top-diameter) screw-hole-wall-thickness)
                          height))
     (->> insert-boss
          (rotate (* 1/2 τ) [1 0 0])
          (translate [0 0 (+ (- height) (- ε))])))))
