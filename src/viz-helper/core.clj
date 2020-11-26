(ns viz-helper.core
  (:require [clojure.string :as str]
            [clojure.edn :as edn]))

;;; constants

(def FILES
  "abcdefgh")

(def PIECES
  [:king :queen :rook :bishop :knight :pawn])

(def DIAG-DIR
  [:nw :ne :sw :se])

;;; conversions

(defn coord->file [coord]
  (nth FILES coord))

(defn file->coord [file]
  (str/index-of FILES file))

(defn coord->rank [coord]
  (inc coord))

(defn rank->coord [rank]
  (dec rank))

(defn coord-pair->square [[x y]]
  (let [file (coord->file x)
        rank (coord->rank y)]
    (str file rank)))

(defn square->coord-pair [square]
  (let [file (nth square 0)
        rank (edn/read-string (str (nth square 1)))]
    [(file->coord file) (rank->coord rank)]))

;;; random generation

(defn rand-coord []
  (rand-int 8))

(defn rand-coord-pair []
  [(rand-coord) (rand-coord)])

(defn rand-square []
  (coord-pair->square (rand-coord-pair)))

(defn rand-piece []
  (rand-nth PIECES))

(defn rand-piece-and-path []
  {:piece (rand-piece)
   :start (rand-square)
   :finish (rand-square)})

(defn rand-dir []
  (rand-nth DIAG-DIR))

;;; viz helpers

(defn square-color [square]
  (let [[x y] (square->coord-pair square)]
    (if (even? (+ x y))
      :dark
      :light)))

(defn positive-diagonal-other-end [square]
  (let [[x y] (square->coord-pair square)]
    (coord-pair->square [(- 7 y) (- 7 x)])))

(defn negative-diagonal-other-end [square]
  (let [[x y] (square->coord-pair square)]
    (coord-pair->square [y x])))

(defn positive-diagonal-for-square [square]
  (let [[x y] (square->coord-pair square)
        z (min x y)]
    (coord-pair->square [(- x z) (- y z)])))

(defn negative-diagonal-for-square [square]
  (let [[x y] (square->coord-pair square)
        z (min x (- 7 y))]
    (coord-pair->square [(- x z) (+ y z)])))

(defn diagonal-arithmetic-problem []
  (let [square (rand-square)
        [x y]  (square->coord-pair square)
        dir    (rand-dir)
        moves  (case dir
                 :nw (rand-int (inc (min x (- 7 y))))
                 :ne (rand-int (inc (min (- 7 x) (- 7 y))))
                 :sw (rand-int (inc (min x y)))
                 :se (rand-int (inc (min (- 7 x) y))))]
    (if (< moves 2)
      (diagonal-arithmetic-problem)
      {:start square
       :dir   dir
       :moves moves})))

(defn knights-moves-for-square [square]
  (let [[x y] (square->coord-pair square)]
    (->>
     [[(dec x) (+ y 2)]
      [(inc x) (+ y 2)]
      [(dec x) (- y 2)]
      [(inc x) (- y 2)]
      [(+ x 2) (dec y)]
      [(+ x 2) (inc y)]
      [(- x 2) (dec y)]
      [(- x 2) (inc y)]]
     (filter (fn [[a b]]
               (and (>= a 0)
                    (<  a 8)
                    (>= b 0)
                    (<  b 8))))
     (mapv coord-pair->square))))

(comment
  )
