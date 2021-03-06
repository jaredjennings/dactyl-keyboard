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
(ns dactyl-keyboard.mvbottom
  "The minimum viable bottom."
  (:refer-clojure :exclude [use import])
  (:require [scad-clj.scad :refer :all]
            [scad-clj.model :refer :all]
            [dactyl-keyboard.util :refer :all]
            [dactyl-keyboard.switch-hole :refer :all]
            [dactyl-keyboard.keycaps :refer :all]
            [dactyl-keyboard.placement :refer :all]
            [dactyl-keyboard.layout :refer :all]
            [dactyl-keyboard.connectors :refer :all]
            [dactyl-keyboard.screw-hole :refer :all]
            [dactyl-keyboard.shph :refer :all]
            [unicode-math.core :refer :all]))


(defn bottom [piece]
  (let [
        for-screw-holes
        (fn [shape]
          (for [hole screw-holes-at]
            (let [[p c r z] hole]
              (->> shape
                   (translate [0 0 z])
                   (translate [0 0 (- (- plate-thickness web-thickness))])
                   ((key-place-fn hole))))))
        plus-shape (screw-hole-pillar-plus screw-hole-pillar-height)
        photo-plus-shape (screw-hole-pillar-plus screw-hole-pillar-height)
        minus-shape (screw-hole-pillar-minus screw-hole-pillar-height)
        base-shape (screw-hole-pillar-base screw-hole-pillar-height)
        plus (apply union (for-screw-holes  plus-shape))
        photo-plus (apply union (for-screw-holes photo-plus-shape))
        minus (apply union (for-screw-holes minus-shape))
        bases (for-screw-holes base-shape)
        base-base-shapes (for-screw-holes (sphere 15))
        base-base-thickness 2
        base-base-bigness 1.5
        bbscale #(scale [base-base-bigness 1 1] %)
        ;; *sigh*
        base-bases (for [b base-base-shapes]
                     (->> (project b)
                     (translate [-30 50 0])
                     (extrude-linear {:height base-base-thickness})
                     (bbscale)
                     (translate [30 -50 0])))
        ;; *SIGH*
        pillar-pillars (for [[bb pb] (map vector base-bases bases)]
                         (hull bb pb))
        ;; *groan*
        base-bases-base (apply triangle-hulls base-bases)
        ]
    (case piece
      :for-printing (difference (union plus pillar-pillars base-bases-base) minus)
      :for-photo (union photo-plus pillar-pillars base-bases-base))))
    
(def bottom-right
  (render (bottom :for-printing)))

(def bottom-right-for-photo
  (render (bottom :for-photo)))
