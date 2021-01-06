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
(ns dactyl-keyboard.switch-hole
  (:refer-clojure :exclude [use import])
  (:require [scad-clj.scad :refer :all]
            [scad-clj.model :refer :all]
            [dactyl-keyboard.util :refer :all]
            [unicode-math.core :refer :all]))

;;;;;;;;;;;;;;;;;
;; Switch Hole ;;
;;;;;;;;;;;;;;;;;

; you may wish to change chosen-single-plate below
(def keyswitch-height 14.1) ;; Was 14.1, then 14.25
(def keyswitch-width 14.1)

(def sa-profile-key-height 12.7)
(def sa-length 18.25)
(def sa-double-length 37.5)

(def plate-thickness 4)
(def web-thickness 3.5)
(def edge-thickness 6)

(def cherry-bezel-width 2.0)
(def mount-width (+ keyswitch-width (* 2 cherry-bezel-width)))
(def mount-height (+ keyswitch-height (* 2 cherry-bezel-width)))



(def alps-width 15.6)
(def alps-notch-width 15.5)
(def alps-notch-height 1)
(def alps-height 13)

(defn cherry-kailh-single-plate-common [nubs]
  (let [top-wall (->> (cube (+ keyswitch-width (* 2 cherry-bezel-width)) cherry-bezel-width plate-thickness)
                      (translate [0
                                  (+ (/ cherry-bezel-width 2) (/ keyswitch-height 2))
                                  (/ plate-thickness 2)]))
        left-wall (->> (cube cherry-bezel-width (+ keyswitch-height (* 2 cherry-bezel-width)) plate-thickness)
                       (translate [(+ (/ cherry-bezel-width 2) (/ keyswitch-width 2))
                                   0
                                   (/ plate-thickness 2)]))
        side-nub (->> (binding [*fn* 30] (cylinder 1 2.75))
                      (rotate (/ π 2) [1 0 0])
                      (translate [(+ (/ keyswitch-width 2)) 0 1])
                      (hull (->> (cube cherry-bezel-width 2.75 plate-thickness)
                                 (translate [(+ (/ cherry-bezel-width 2) (/ keyswitch-width 2))
                                             0
                                             (/ plate-thickness 2)]))))
                                        ; thanks to FSund - see
                                        ; https://github.com/adereth/dactyl-keyboard/issues/56#issuecomment-376463735
        plate-half (union top-wall left-wall (if nubs (with-fn 100 side-nub)))]
    (union plate-half
           (->> plate-half
                (mirror [1 0 0])
                (mirror [0 1 0])))))
(def cherry-single-plate (cherry-kailh-single-plate-common true))
(def kailh-single-plate  (cherry-kailh-single-plate-common false))

(def cherry-blank-single-plate
  (->>
    (cube (+ keyswitch-width (* 2 cherry-bezel-width))
          (+ keyswitch-height (* 2 cherry-bezel-width))
          plate-thickness)
    (translate [0 0 (/ plate-thickness 2)])))

(def alps-single-plate
  (let [top-wall (->> (cube (+ keyswitch-width 3) 2.2 plate-thickness)
                      (translate [0
                                  (+ (/ 2.2 2) (/ alps-height 2))
                                  (/ plate-thickness 2)]))
        left-wall (union (->> (cube 1.5 (+ keyswitch-height 3) plate-thickness)
                              (translate [(+ (/ 1.5 2) (/ 15.6 2))
                                          0
                                          (/ plate-thickness 2)]))
                         (->> (cube 1.5 (+ keyswitch-height 3) 1.0)
                              (translate [(+ (/ 1.5 2) (/ alps-notch-width 2))
                                          0
                                          (- plate-thickness
                                             (/ alps-notch-height 2))]))
                         )
        plate-half (union top-wall left-wall)]
    (union plate-half
           (->> plate-half
                (mirror [1 0 0])
                (mirror [0 1 0])))))

(def chosen-single-plate kailh-single-plate)
(def chosen-blank-single-plate cherry-blank-single-plate)



                                        ; thumb plates

(def double-plates
  (let [plate-height (/ (- sa-double-length mount-height) 2)
        top-plate (->> (cube mount-width plate-height web-thickness)
                       (translate [0 (/ (+ plate-height mount-height) 2)
                                   (- plate-thickness (/ web-thickness 2))]))
        stabilizer-cutout (union (->> (cube keyswitch-width 3.5 web-thickness)
                                      (translate [0 12 (- plate-thickness (/ web-thickness 2))])
                                      (color [1 0 0 1/2]))
                                 ;; not sure why this lower cube was
                                 ;; wider, but it caused a weak
                                 ;; spot (jaredjennings#30). making it
                                 ;; narrower.
                                 (->> (cube (+ keyswitch-width 0) 3.5 web-thickness)
                                      (translate [0 12 (- plate-thickness (/ web-thickness 2) 1.4)])
                                      (color [1 0 0 1/2])))
        top-plate (difference top-plate stabilizer-cutout)]
    (union top-plate (mirror [0 1 0] top-plate))))

(def double-plates-blank
  (let [plate-height (/ (- sa-double-length mount-height) 2)
        top-plate (->> (cube mount-width plate-height web-thickness)
                       (translate [0 (/ (+ plate-height mount-height) 2)
                                   (- plate-thickness (/ web-thickness 2))]))]
    (union top-plate (mirror [0 1 0] top-plate))))
