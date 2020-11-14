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
(ns dactyl-keyboard.adafruit-usb
  (:refer-clojure :exclude [use import])
  (:require [scad-clj.scad :refer :all]
            [scad-clj.model :refer :all]
            [dactyl-keyboard.util :refer :all]
            [unicode-math.core :refer :all]
            [dactyl-keyboard.switch-hole :refer [mount-height
                                                 plate-thickness]]
            [dactyl-keyboard.shph
             :refer [sides-radius
                     sides-downness]]
            [dactyl-keyboard.layout :refer [key-place-fn usb-socket-at
                                            usb-socket-region]]
            [dactyl-keyboard.sides :refer [partial-sides]]
            [dactyl-keyboard.placement :refer [key-place]]))

;; https://www.cs.jhu.edu/~carlson/download/datasheets/Micro-USB_1_01.pdf
;;
;; Micro USB Cables and Connectors Specification 1.01 Figure 4-5, Page
;; 18.  An overmold is the plastic that goes around the metal and
;; wires of a usb connector. The standard overmold size is very
;; chunky, and every actual cable should be smaller.
(def micro-b-overmold-width 11)
(def micro-b-overmold-height 9)

;; Adafruit Panel Mount Extension USB Cable - Micro B Male to Micro B Female [ADA3258]
;;
;; "4-40 screws... 18mm apart." 4-40 -> 0.112" major diameter -> 2.844mm.
(def adafruit-usb-screws-diameter 3)
(def adafruit-usb-screws-center 18)

(def adafruit-usb-plate-thickness 2)
(def adafruit-usb-cutout-depth 20)

(def adafruit-usb-cutout
  (let [overmold-hole (cube micro-b-overmold-width
                            adafruit-usb-cutout-depth
                            micro-b-overmold-height)
        screw-hole-1 (->>
                      (with-fn 12
                        (cylinder (/ adafruit-usb-screws-diameter 2)
                                  adafruit-usb-cutout-depth))
                      (rotate (/ π 2) [1 0 0])
                      (translate [(/ adafruit-usb-screws-center 2)
                                  0 0]))
        screw-hole-2 (mirror [-1 0 0] screw-hole-1)]
    (union overmold-hole screw-hole-1 screw-hole-2)))

(defn adafruit-usb-plate-shape [thickness r]
  (let [screw-plate-1 (->>
                       (cylinder r thickness)
                       (rotate (* 1/4 τ) [1 0 0])
                       (translate
                        [(* 1/2  adafruit-usb-screws-center) 0 0]))
        screw-plate-2 (mirror [-1 0 0] screw-plate-1)
        joiner (cube adafruit-usb-screws-center thickness (* 2 r))]
    (union screw-plate-1 screw-plate-2 joiner)))

(def adafruit-usb-plate
  (adafruit-usb-plate-shape adafruit-usb-plate-thickness
                            (* 1/2 (* 3/2 micro-b-overmold-height))))

(def adafruit-usb-region
  (adafruit-usb-plate-shape (* 4 adafruit-usb-plate-thickness)
                            (* 4/3 (* 1/2 (* 3/2 micro-b-overmold-height)))))


(defn usb-cutout-place [shape]
  (->> shape
       (translate [0 (/ mount-height 2) 0])
       (translate [0 sides-radius (+ sides-downness
                                     (- plate-thickness)
                                     -3 ;; oh heck with it.
                                     )])
       (translate [0 (- adafruit-usb-plate-thickness) 0]) ; fudge this
       ((key-place-fn (rest usb-socket-at)))))

(def usb-intersect
  (intersection
   (usb-cutout-place adafruit-usb-region)
   (partial-sides usb-socket-region)))

(def usb-nice-plate
  (hull usb-intersect (usb-cutout-place adafruit-usb-plate)))
