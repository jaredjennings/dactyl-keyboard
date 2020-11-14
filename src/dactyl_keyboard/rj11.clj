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
(ns dactyl-keyboard.rj11
  (:refer-clojure :exclude [use import])
  (:require [scad-clj.scad :refer :all]
            [scad-clj.model :refer :all]
            [dactyl-keyboard.util :refer :all]
            [dactyl-keyboard.placement :refer [key-place]]
            [dactyl-keyboard.layout :refer [key-place-fn rj11-socket-at rj11-socket-region]]
            [dactyl-keyboard.switch-hole :refer [mount-height
                                                 plate-thickness]]
            [dactyl-keyboard.shph
             :refer [sides-radius
                     sides-downness]]
            [dactyl-keyboard.sides
             :refer [partial-sides]]
            [unicode-math.core :refer :all]))

;; https://www.mouser.com/ds/2/18/61835-1003706.pdf
(def rj11-face-width 11.18)
(def rj11-face-height 15.87)
(def rj11-body-width 13.72)
(def rj11-cutout-depth 20)
(def rj11-plate-thickness 12)
(def rj11-bezel 3)


(def rj11-cutout
  (cube rj11-body-width rj11-cutout-depth rj11-face-height))

(defn rj11-plate-shape [margin thickness]
  (cube (+ rj11-body-width margin)
        thickness
        (+ rj11-face-height margin)))

(defn rj11-cutout-place [shape]
  (->> shape
       (translate [0 (/ mount-height 2) 0])
       (translate [0 sides-radius (+ sides-downness
                                     (- plate-thickness)
                                     -3
                                     )])
       (translate [0 (* -3/5 rj11-plate-thickness) 0]) ; fudged
       ((key-place-fn (rest rj11-socket-at)))))

(def rj11-nice-plate
  (let [rj11-plate (rj11-plate-shape rj11-bezel rj11-plate-thickness)
        rj11-region (rj11-plate-shape (* 2 rj11-bezel) rj11-cutout-depth)
        rj11-intersect (intersection
                        (rj11-cutout-place rj11-region)
                        (partial-sides rj11-socket-region))]
    (hull rj11-intersect (rj11-cutout-place rj11-plate))))
