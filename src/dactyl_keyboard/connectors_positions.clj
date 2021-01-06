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
(ns dactyl-keyboard.connectors-positions
  (:refer-clojure :exclude [use import])
  (:require [scad-clj.scad :refer :all]
            [scad-clj.model :refer :all]
            [dactyl-keyboard.switch-hole :refer [mount-width mount-height]]
            ))

(def post-size 0.1)
(def post-adj (/ post-size 2))
(def outer-edge-distance 3)

(defn switch-hole-corners [direction]
  (let [z 0
        N (- (/ mount-height 2) post-adj)
        S (+ (/ mount-height -2) post-adj)
        E (- (/ mount-width 2) post-adj)
        W (+ (/ mount-width -2) post-adj)
        offsets {:n [0 N z]
                 :s [0 S z]
                 :e [E 0 z]
                 :w [W 0 z]
                 :nw [W N z]
                 :ne [E N z]
                 :sw [W S z]
                 :se [E S z]}]
    #(translate (offsets direction) %)))

(def n (switch-hole-corners :n))
(def s (switch-hole-corners :s))
(def e (switch-hole-corners :e))
(def w (switch-hole-corners :w))
(def nw (switch-hole-corners :nw))
(def sw (switch-hole-corners :sw))
(def ne (switch-hole-corners :ne))
(def se (switch-hole-corners :se))

(defn outer-edge-corners [direction]
  (let [z 0
        N (- (/ mount-height 2) post-adj)
        S (+ (/ mount-height -2) post-adj)
        E (- (/ mount-width 2) post-adj)
        W (+ (/ mount-width -2) post-adj)
        ON (+ (- (/ mount-height 2) post-adj) outer-edge-distance)
        OS (- (+ (/ mount-height -2) post-adj) outer-edge-distance)
        OE (+ (- (/ mount-width 2) post-adj) outer-edge-distance)
        OW (- (+ (/ mount-width -2) post-adj) outer-edge-distance)
        offsets {:hsw [OW S z]
                 :hnw [OW N z]
                 :vnw [W ON z]
                 :vne [E ON z]
                 :hne [OE N z]
                 :hse [OE S z]
                 :vse [E OS z]
                 :vsw [W OS z]}]
    #(translate (offsets direction) %)))

(def oevnw (outer-edge-corners :vnw))
(def oevne (outer-edge-corners :vne))
(def oevsw (outer-edge-corners :vsw))
(def oevse (outer-edge-corners :vse))
(def oehnw (outer-edge-corners :hnw))
(def oehne (outer-edge-corners :hne))
(def oehsw (outer-edge-corners :hsw))
(def oehse (outer-edge-corners :hse))
