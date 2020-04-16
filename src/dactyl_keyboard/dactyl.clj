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
(ns dactyl-keyboard.dactyl
  (:refer-clojure :exclude [use import])
  (:require [scad-clj.scad :refer [write-scad]]
            [scad-clj.model :as m]
            [dactyl-keyboard.util :refer :all]
            [dactyl-keyboard.switch-hole :refer :all]
            [dactyl-keyboard.keycaps :refer :all]
            [dactyl-keyboard.placement :refer :all]
            [dactyl-keyboard.layout :refer :all]
            [dactyl-keyboard.connectors :refer :all]
            [dactyl-keyboard.sides-connectors :refer :all]
            [dactyl-keyboard.sides :refer :all]
            [dactyl-keyboard.bottom :refer :all]
            [dactyl-keyboard.sides-pieces :refer :all]
            [dactyl-keyboard.screw-hole :refer :all]
            [unicode-math.core :refer :all]
            [dactyl-keyboard.half-circle-connectors :refer :all]
            [dactyl-keyboard.adafruit-usb :refer :all]
            [dactyl-keyboard.rj11 :refer :all]))

(def ^:dynamic *scad-being-written* "(unknown)")

(defn basename [filename] (last (clojure.string/split filename #"/")))

(defn ensure-line-in-file [line filename]
  (if (.exists (clojure.java.io/file filename))
    (let [lines (clojure.string/split-lines (slurp filename))]
      (if (not (some #{(clojure.string/trim line)} lines))
        (spit filename line :append true)))
    (spit filename line)))

(defn use [library]
  (do
   (ensure-line-in-file (format "%s: %s\n" (basename *scad-being-written*) library)
                        "things/.deps")
   (m/use library)))

(defn import [file & args]
  (let [fb (basename file)] ; the import statement is going to import
                            ; an stl in the things directory, and it
                            ; is being written in an scad in the
                            ; things directory
    (do
      (ensure-line-in-file (format "%s: %s\n" (basename *scad-being-written*) fb)
                           "things/.deps")
      (apply m/import (cons fb args)))))


(def thumb
  (m/union
   thumb-connectors
   (thumb-layout (m/rotate (/ π 2) [0 0 1] chosen-single-plate))
   (thumb-place 0 -1/2 double-plates)
   (thumb-place 1 -1/2 double-plates)))

(def thumb-blanks
  (m/union
   (thumb-layout (m/rotate (/ π 2) [0 0 1] chosen-blank-single-plate))
   (thumb-place 0 -1/2 double-plates-blank)
   (thumb-place 1 -1/2 double-plates-blank)))




;;;;;;;;;;;;;;;;;;
;; Final Export ;;
;;;;;;;;;;;;;;;;;;

(defn dactyl-top-right-plusses [key-pieces]
  ; agh i made bad names and now i pay for it
  (let [pieces-of-pieces (map vector
                              key-pieces
                              connectors-inside-fingerpieces
                              (for [cols columns-pieces]
                                (let [teensy-column (nth teensy-bracket-at 1)]
                                  (if (and (>= teensy-column (first cols))
                                           (<= teensy-column (last cols)))
                                    (->> screw-hole-for-teensy
                                         ((key-place-fn teensy-bracket-at)))))))]
    (for [pieces-of-this-piece pieces-of-pieces]
      (apply m/union pieces-of-this-piece))))

(defn dactyl-top-right-minuses [key-pieces]
  (let [pieces-of-pieces (map vector
                              screw-holes-in-fingerpieces)]
    (for [pieces-of-this-piece pieces-of-pieces]
      (apply m/union pieces-of-this-piece))))

(defn dactyl-top-right-pieces [key-pieces]
  (for [[plus minus] (map vector
                          (dactyl-top-right-plusses key-pieces)
                          (dactyl-top-right-minuses key-pieces))]
        (m/difference plus minus)))

(def dactyl-top-right-thumb
  (m/difference
   thumb
   screw-holes-in-thumb))

(def define-sides-with-right-ports
  (m/define-module "SidesWithRightPorts"
    (m/with-fn 12
      (m/difference
       (m/union sides-right
              usb-nice-plate
              rj11-nice-plate)
       (usb-cutout-place adafruit-usb-cutout)
       (rj11-cutout-place rj11-cutout)))))

(def define-sides-with-left-ports
  (m/define-module "SidesWithLeftPorts"
    (m/with-fn 12
      (m/difference
       (m/union sides-right
              rj11-nice-plate)
       (rj11-cutout-place rj11-cutout)))))


;; Put some keywords in skip-tags, and say-spit calls with those tags
;; will not be evaluated. If you temporarily don't care about some
;; parts, this may help you iterate faster. Usually this should be
;; #{}.
(def skip-tags #{})
;; Put some keywords in emit-tags, and only say-spit calls with those
;; tags will be evaluated. If you are iterating on just one or two
;; parts, this may help you iterate faster. Usually this should be
;; #{}.
(def emit-tags #{})

(defn emit? [tags]
  (cond
    (some emit-tags tags) true
    (and (empty? emit-tags)
         (not-any? skip-tags tags)) true
    :else false))

(defn make-filename [tags & {:keys [ext] :or {ext ".scad"}}]
  (let [tag-abbrevs {:debugmodel "debug-"
                     :intermediate "i-"
                     :piece "dm-"
                     :frame "fra"
                     :bottom "bot"
                     :sides "sid"
                     :legs "leg"
                     :right "r-"
                     :left "l-"
                     :thumb "th"}
        stringify (fn [x] (cond
                            (integer? x) (format "%02d" x)
                            (keyword? x) (or (tag-abbrevs x) (name x))))
        filename (format "things/%s%s"
                         (clojure.string/join (map stringify tags))
                         ext)]
    filename))

(defmacro say-spit [tags & body]
  (if (emit? tags)
    `(let [filename# (make-filename ~tags)]
       (do
         (binding [*scad-being-written* filename#]
           (print (format "%s  emitting  %s\n" ~tags filename#))
           (spit filename# ~@body))))
    `(print (format "%s *SKIPPING* %s\n" ~tags (make-filename ~tags)))))

(say-spit [:debugmodel :single-plate]
      (write-scad chosen-single-plate))

(say-spit [:piece :right :frame :thumb]
          (write-scad
           (use "key-place.scad")
           (use "eggcrate.scad")
           dactyl-top-right-thumb
           (sides-connectors-thumb-from-notation sides-frame-joints)))

(say-spit [:piece :left :frame :thumb]
          (write-scad
           (use "key-place.scad")
           (use "eggcrate.scad")
           (m/mirror [1 0 0]
                   (m/union
                    dactyl-top-right-thumb
                    (sides-connectors-thumb-from-notation sides-frame-joints)))))

(doseq [[partno part1 part2]
        (map vector (range)
             (dactyl-top-right-pieces key-holes-pieces)
             (sides-connectors-frame-from-notation sides-frame-joints))]
  (do
    (say-spit [:piece :right :frame partno]
            (write-scad
             (use "key-place.scad")
             (use "eggcrate.scad")
             (m/union part1 part2)))
    (say-spit [:piece :left :frame partno]
            (write-scad
             (use "key-place.scad")
             (use "eggcrate.scad")
             (m/mirror [1 0 0]
                     (m/union part1 part2))))))

(say-spit [:debugmodel :right :frame :all]
          (write-scad
           (use "key-place.scad")
           (m/union dactyl-top-right-thumb
                  (apply m/union (dactyl-top-right-pieces key-holes-pieces))
                  caps
                  thumbcaps)))

(say-spit [:debugmodel :left :frame :all]
          (write-scad
           (use "key-place.scad")
           (m/mirror [1 0 0]
                   (m/union dactyl-top-right-thumb
                          (apply m/union (dactyl-top-right-pieces key-holes-pieces))
                          caps
                          thumbcaps))))

(def sides-slices-right
  (pieces-with-x-pins-and-holes-faster (* sides-radius 3/4)
                                the-sides-slice-joints
                                sides-slice-intersects
                                (m/call-module "SidesWithRightPorts")
                                sides-regions))

(def sides-slices-left
  (pieces-with-x-pins-and-holes-faster (* sides-radius 3/4)
                                the-sides-slice-joints
                                sides-slice-intersects
                                (m/call-module "SidesWithLeftPorts")
                                sides-regions))

(doseq [[partno part1 part2] (map vector (range)
                                  sides-slices-right
                                  (sides-connectors-sides-from-notation
                                   sides-frame-joints
                                   sides-slices-right))]
  (say-spit [:piece :right :sides partno]
            (write-scad
             (use "key-place.scad")
             (use "eggcrate.scad")
             define-sides-with-right-ports
             (m/union part1 part2
                    ;; for reference when adjusting sides downness:
                    #_(m/union dactyl-top-right-thumb
                           (apply m/union (dactyl-top-right-pieces
                                         key-holes-pieces)))))))

(doseq [[partno part1 part2] (map vector (range)
                                  sides-slices-left
                                  (sides-connectors-sides-from-notation
                                   sides-frame-joints
                                   sides-slices-right))]
  (say-spit [:piece :left :sides partno]
            (write-scad
             (use "key-place.scad")
             (use "eggcrate.scad")
             define-sides-with-left-ports
             (m/mirror [1 0 0] (m/union part1 part2)))))

(say-spit [:intermediate :right :bottom :all]
          (write-scad
           (use "key-place.scad")
           (use "eggcrate.scad")
           (m/union
            bottom-right)
            #_(m/union dactyl-top-right-thumb
                   (apply m/union
                          (dactyl-top-right-pieces key-holes-pieces)))))

(defn import-bottom-right []
  (let [bottom-right-stl-filename
        (make-filename [:intermediate :right :bottom :all] :ext ".stl")]
    (import bottom-right-stl-filename)))

(say-spit [:intermediate :left :bottom :all]
          (write-scad
           (use "key-place.scad")
           (use "eggcrate.scad")
           (m/mirror [1 0 0]
                   (m/union
                    (import-bottom-right)))))

(say-spit [:debugmodel :splits]
          (write-scad
           (use "key-place.scad")
           (m/union
            (m/union dactyl-top-right-thumb
                   (apply m/union (dactyl-top-right-pieces key-holes-pieces)))
            sides-slice-intersects
            )))

(say-spit [:debugmodel :joins]
          (write-scad
           (use "key-place.scad")
           define-sides-with-right-ports
           (m/union
            (m/union dactyl-top-right-thumb
                   (apply m/union (dactyl-top-right-pieces key-holes-pieces)))
            (map #(% (m/rotate (* 1/4 τ) [0 1 0] (m/cylinder [10 0] 10))) the-sides-slice-joints))))

(say-spit [:debugmodel :right :keys]
          (write-scad
           (use "key-place.scad")
           (m/union
            (m/union caps thumbcaps))))

(say-spit [:debugmodel :left :keys]
          (write-scad
           (use "key-place.scad")
           (m/mirror [1 0 0] (m/union caps thumbcaps))))

(say-spit [:debugmodel :photo]
          (write-scad
           (use "key-place.scad")
           (use "eggcrate.scad")
           define-sides-with-right-ports
           (m/union
            sides-right
            (import-bottom-right)
            (m/union caps thumbcaps)
            (m/union dactyl-top-right-thumb
                   (apply m/union (dactyl-top-right-pieces key-holes-pieces)))
            )))

(say-spit [:debugmodel :teensy-holder-clearance-check]
          (write-scad
           (use "key-place.scad")
           (use "eggcrate.scad")
           (use "teensy-holder.scad")
           (m/union
            (->> (m/call-module "teensy_holder_piece_a")
                 (m/rotate (* 1/2 τ) [0 0 1])
                 (m/translate [0 0 (- teensy-screw-hole-height)])
                 ((key-place-fn teensy-bracket-at)))
            (import-bottom-right))))

(say-spit [:debugmodel :right :bottom :all]
          (write-scad
           (use "key-place.scad")
           (use "eggcrate.scad")
           (import-bottom-right)))

(say-spit [:debugmodel :right :legs :all]
          (write-scad
           (use "key-place.scad")
           (use "eggcrate.scad")
           (legs true)
           (apply m/union (legs false))
           ))

(doseq [[partno leg] (map vector (range) (legs false))]
  (say-spit [:piece :right :legs partno]
            (write-scad
             (use "key-place.scad")
             (use "eggcrate.scad")
             leg)))

(doseq [[partno leg] (map vector (range) (legs false))]
  (say-spit [:piece :left :legs partno]
            (write-scad
             (use "key-place.scad")
             (use "eggcrate.scad")
             (m/mirror [1 0 0] leg))))

(say-spit [:debugmodel :left :keys :all]
          (write-scad
           (use "key-place.scad")
           (m/intersection
            (m/union
             (apply m/union
                    (for [column columns
                          row rows
                          :when (finger-has-key-place-p column row)]
                      (m/hull (key-place column row (sa-cap 1))
                              (m/translate [0 0 -100]
                                           (key-place column row (sa-cap 1))))))
             (m/extrude-linear {:height 10}
                               (m/project
                                (apply m/hull
                                       (for [column columns
                                             row rows
                                             :when (finger-has-key-place-p column row)]
                                         (->> (sa-cap 1)
                                              (m/scale [2 2 2])
                                              (key-place column row)
                                              (m/translate [0 0 -100])))))))
            (m/translate [0 0 100] (m/cube 500 500 200)))))

(say-spit [:piece :screw-hole-top]
          (write-scad
           (use "eggcrate.scad")
           (screw-hole-pillar-upper screw-hole-pillar-height)))
