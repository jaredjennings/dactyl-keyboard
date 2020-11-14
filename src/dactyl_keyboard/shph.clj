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

(ns dactyl-keyboard.shph
  "This one variable that two modules depended on one time. And some
  other variables that that one depended on.")
            
;; if you change this you will likely need to change the
;; usb-cutout-place function and the rj11-cutout-place function
;;
;; sides-downness 0 puts the center of the sides shapes on the level
;; of the bottom of the frame.
(def sides-downness 0)
(def sides-thickness 3)
;; outer radius of sides
(def sides-radius 10)

(def bottom-distance 0)
(def screw-hole-pillar-height (+ (Math/abs (float bottom-distance))
                                 (- sides-radius sides-thickness)
                                 ;; they are cut off at the bottom
                                 ;; so add slop
                                 1))
