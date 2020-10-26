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
(ns dactyl-keyboard.connectors
  (:refer-clojure :exclude [use import])
  (:require [scad-clj.scad :refer :all]
            [scad-clj.model :refer :all]
            [dactyl-keyboard.util :refer :all]
            [dactyl-keyboard.switch-hole :refer :all]
            [dactyl-keyboard.placement :refer :all]
            [dactyl-keyboard.layout :refer :all]
            [dactyl-keyboard.screw-hole :refer :all]
            [dactyl-keyboard.connectors-positions :refer :all]
            ))

;;;;;;;;;;;;;;;;;;;;
;; Web Connectors ;;
;;;;;;;;;;;;;;;;;;;;

#_(def web-post (->> (cube post-size post-size web-thickness)
                   (translate [0 0 (+ (/ web-thickness -2)
                                      plate-thickness)])))
(def web-post (call-module "WebPost"))
(def above-web-post (call-module "AboveWebPost"))

;; these are like web-posts, but lying down.  horizontal or
;; vertical (EW / NS key-place-wise); bottom or top (key actuation
;; axis wise); and which way it sticks out (north, south, east, west
;; key-place-wise).
(def web-log-hbw (call-module "WebLogHBW"))
(def web-log-hbe (call-module "WebLogHBE"))
(def web-log-htw (call-module "WebLogHTW"))
(def web-log-hte (call-module "WebLogHTE"))
(def web-log-vbn (call-module "WebLogVBN"))
(def web-log-vbs (call-module "WebLogVBS"))
(def web-log-vtn (call-module "WebLogVTN"))
(def web-log-vts (call-module "WebLogVTS"))


(defn row-connector [column row post]
  (triangle-hulls
     (key-place (inc column) row (nw post))
     (key-place column row (ne post))
     (key-place (inc column) row (sw post))
     (key-place column row (se post))))

(defn row-connectors [column post]
  (for [row rows
        :when (and (finger-has-key-place-p column row)
                   (finger-has-key-place-p (inc column) row))]
    (row-connector column row post)))

(defn diagonal-connector [column row post]
  (triangle-hulls
     (key-place column row (se post))
     (key-place column (inc row) (ne post))
     (key-place (inc column) row (sw post))
     (key-place (inc column) (inc row) (nw post))))

(defn diagonal-connectors [column post]
  (for [row (drop-last rows)
        :when (and (finger-has-key-place-p (inc column) (inc row))
                   (finger-has-key-place-p column (inc row)))]
    (diagonal-connector column row post)))

(defn column-connector [column row post]
  (triangle-hulls
     (key-place column row (sw post))
     (key-place column row (se post))
     (key-place column (inc row) (nw post))
     (key-place column (inc row) (ne post))))

(defn column-connectors [column post]
  (for [row (drop-last rows)
        :when (finger-has-key-place-p column (inc row))]
    (column-connector column row post)))

(defn connectors-inside-fingerpieces-helper [post]
  (let
      [connectors-for-columns
       (fn [columns]
         (apply union
                (for [column (drop-last columns)]
                  (concat
                   (row-connectors column post)
                   (diagonal-connectors column post)))
                 (for [column columns]
                   (column-connectors column post))))]
    (map connectors-for-columns columns-pieces)))

(def connectors-inside-fingerpieces
  (connectors-inside-fingerpieces-helper web-post))

(def above-all-connectors-inside-fingerpieces
  (apply union
         (connectors-inside-fingerpieces-helper above-web-post)))

;; caution: this only works because my keyboard is entirely composed
;; of 1u keys. otherwise it would have to be made more flexible, like
;; sa-cap is.
(def above-key
  (hull
   (translate [0 0 plate-thickness] chosen-blank-single-plate)
   (translate [0 0 (* plate-thickness 5)] chosen-blank-single-plate)))

(def above-key-blanks (key-shapes-for-columns above-key columns))
(def above-finger-surface
  (union above-key-blanks above-all-connectors-inside-fingerpieces))

(defn connectors-inside-thumb-helper [post]
  (apply union
         (concat
          (for [column [1 2] row [-1 0]]
            (triangle-hulls (thumb-place column row (se post))
                            (thumb-place column row (ne post))
                            (thumb-place (dec column) row (sw post))
                            (thumb-place (dec column) row (nw post))))
          (for [column [0 1 2] row [0]]
            (triangle-hulls
             (thumb-place column row (sw post))
             (thumb-place column row (se post))
             (thumb-place column (dec row) (nw post))
             (thumb-place column (dec row) (ne post))))
          (for [column [1 2] row [0]]
            (triangle-hulls
              (thumb-place column row (se post))
              (thumb-place (dec column) row (sw post))
              (thumb-place column (dec row) (ne post))
              (thumb-place (dec column) (dec row) (nw post)))))))

(def connectors-inside-thumb
  (connectors-inside-thumb-helper web-post))

(def above-connectors-inside-thumb
  (connectors-inside-thumb-helper above-web-post))

(def above-thumb-blanks (thumb-layout above-key))
(def above-thumb-surface
  (union above-thumb-blanks above-connectors-inside-thumb))

(defn for-fingerpiece-screwholes
  [shape & {:keys [minus] :or {minus nil}}]
  (for [columns columns-pieces]
    (difference
     (apply union
            (for [place screw-holes-at]
              (let [[p c r z] place]
                (if (and (= p :k)
                         (>= c (first columns))
                         (<= c (last columns)))
                  (->> shape
                       (translate [0 0 z])
                       ((key-place-fn place)))))))
     minus)))
  

(def screw-holes-in-fingerpieces-minus
  (for-fingerpiece-screwholes
   (color [1 0 0] frame-screw-hole-minus)))

(def screw-holes-in-fingerpieces-plus
  (for-fingerpiece-screwholes
   frame-screw-hole-plus
   :minus above-finger-surface))

(defn for-screw-holes-in-thumb
  [shape & {:keys [minus] :or {minus nil}}]
  (difference
   (apply union
          (for [place screw-holes-at]
            (let [[p c r z] place]
              (if (= p :t)
                (->> shape
                     (translate [0 0 z])
                     ((key-place-fn place)))))))
   minus))

(def screw-holes-in-thumb-plus
  (for-screw-holes-in-thumb
   frame-screw-hole-plus
   :minus above-thumb-surface))

(def screw-holes-in-thumb-minus
  (for-screw-holes-in-thumb
   (color [1 0 0] frame-screw-hole-minus)))

;; we do not have flexibility on our post shape here, as above,
;; because that flexibility is needed to construct the area above the
;; web so we can cleanly cut off the tops of the screw holes. but
;; there are no screw holes in the thumb-to-finger-connector.
(def thumb-to-finger-connector
  (let [
        plate-height (/ (- sa-double-length mount-height) 2)
        thumb-tl (->> (nw web-post)
                      (translate [0 plate-height 0]))
        thumb-bl (->> (sw web-post)
                      (translate [0 (- plate-height) 0]))
        thumb-tr (->> (ne web-post)
                      (translate [0 plate-height 0]))
        thumb-br (->> (se web-post)
                      (translate [0 (- plate-height) 0]))]
    (union
     
     ;;Connecting the thumb to everything
     (triangle-hulls
      (hull (key-place 2 4 (se web-post)) (key-place 2 4 (se web-log-vbs)))
      (hull (thumb-place 0 -1 (se web-post)) (thumb-place 0 -1 (se web-log-hbe)))
      (hull (key-place 2 4 (sw web-post)) (key-place 2 4 (sw web-log-vbs)))
      (hull (thumb-place 0 -1 (ne web-post)) (thumb-place 0 -1 (ne web-log-hbe)))
      (hull (key-place 2 4 (sw web-post)) (key-place 2 4 (sw web-log-vbs)))
      (hull (thumb-place 0 0 (se web-post)) (thumb-place 0 0 (se web-log-hbe)))
      (hull (key-place 2 4 (sw web-post)) (key-place 2 4 (sw web-log-hbw)))
      (thumb-place 0 0 (ne web-post)))
     
     (triangle-hulls
      (key-place 2 3 (sw web-post))
      (key-place 2 4 (nw web-post))
      (key-place 1 3 (se web-post)))
     (triangle-hulls
      (thumb-place 0 0 (n web-post))
      (hull (key-place 1 3 (s web-post)) (key-place 1 3 (s web-log-vbs)))
      (hull (thumb-place 0 0 (nw web-post)) (thumb-place 0 0 (nw web-log-vtn)))
      (hull (key-place 1 3 (sw web-post)) (key-place 1 3 (sw web-log-vbs)))
      (hull (thumb-place 1 0 (ne web-post)) (thumb-place 1 0 (ne web-log-vtn)))
      (hull (key-place 0 3 (sw web-post)) (key-place 0 3 (sw web-log-vbs)))
      (hull (key-place 0 3 (sw web-post)) (key-place 0 3 (sw web-log-vbs)))
      (hull (thumb-place 1 0 (ne web-post)) (thumb-place 1 0 (ne web-log-vtn)))
      (hull (thumb-place 1 0 (ne web-post)) (thumb-place 1 0 (ne web-log-vtn)))
      (thumb-place 1 0 (nw web-post))))))

(def thumb-connectors
  (union thumb-to-finger-connector connectors-inside-thumb))
