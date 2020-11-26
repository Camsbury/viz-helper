(ns viz-helper.core
  (:require [clojure.string :as str]
            [clojure.edn :as edn]))

;;; constants

(def FILES
  "abcdefgh")

(def PIECES
  [:king :queen :rook :bishop :knight])

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

;;; viz helpers

(defn square-color [square]
  (let [[x y] (square->coord-pair square)]
    (if (even? (+ x y))
      :dark
      :light)))

(defn diagonal-other-end [{:keys [square slope]}]
(let [[x y] (square->coord-pair square)]
  (case slope
    :pos (coord-pair->square [(- 7 y) (- 7 x)])
    :neg (coord-pair->square [y x]))))

(defn positive-diagonal-for-square [square]
  (let [[x y] (square->coord-pair square)
        z (min x y)]
    (coord-pair->square [(- x z) (- y z)])))

(defn negative-diagonal-for-square [square]
  (let [[x y] (square->coord-pair square)
        z (min x (- 7 y))]
    (coord-pair->square [(- x z) (+ y z)])))

;;; random generation

(defn rand-coord []
  (rand-int 8))

(defn rand-coord-pair []
  [(rand-coord) (rand-coord)])

(defn rand-square []
  (coord-pair->square (rand-coord-pair)))

(defn rand-piece []
  (rand-nth PIECES))

(defn rand-path-for-piece [piece]
  (let [start (rand-square)
        finish (rand-square)]
    (if (or
         (= start finish)
         (and (= piece :bishop)
              (not=
               (square-color start)
               (square-color finish))))
      (rand-piece-and-path)
      [start finish])))

(defn rand-piece-and-path []
  (let [piece           (rand-piece)
        [start finish]  (rand-path-for-piece piece)]
    {:piece piece
     :start start
     :finish finish}))

(defn rand-dir []
  (rand-nth DIAG-DIR))

(defn rand-diagonal-end []
  (let [edge (rand-nth [0 7])
        coords (shuffle [(rand-coord) edge])]
    {:slope (rand-nth [:pos :neg])
     :square (coord-pair->square coords)}))

;;; problems and solutions

(defn file-arithmetic-problem []
  (let [coord (rand-coord)
        operation (rand-nth [:plus :minus])
        number (case operation
                 :plus (rand-int (- 7 coord))
                 :minus (rand-int coord))]
    (if (< number 2)
      (file-arithmetic-problem)
      {:file (coord->file coord)
       :operation operation
       :number number})))

(defn file-arithmetic-solution
  [{:keys [file operation number]}]
  (let [op (case operation
             :plus +
             :minus -)]
    (-> file file->coord (op number) coord->file)))

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

(defn diagonal-arithmetic-solution
  [{:keys [start dir moves]}]
  (let [move (fn [[x y]]
               (case dir
                 :nw [(- x moves) (+ y moves)]
                 :ne [(+ x moves) (+ y moves)]
                 :sw [(- x moves) (- y moves)]
                 :se [(+ x moves) (- y moves)]))]
    (-> start
        square->coord-pair
        move
        coord-pair->square)))

(defn knights-moves-for-square [square]
  (let [[x y] (square->coord-pair square)]
    (->>
     [[(inc x) (+ y 2)]
      [(+ x 2) (inc y)]
      [(+ x 2) (dec y)]
      [(inc x) (- y 2)]
      [(dec x) (- y 2)]
      [(- x 2) (dec y)]
      [(- x 2) (inc y)]
      [(dec x) (+ y 2)]]
     (filter (fn [[a b]]
               (and (>= a 0)
                    (<  a 8)
                    (>= b 0)
                    (<  b 8))))
     (mapv coord-pair->square))))

;;; puzzle players

(defn puzzle-player
  [{:keys [question-fn answer-fn]}]
  (let [state (atom nil)]
    (fn []
      (let [{:keys [question]} @state]
        (if question
          (do
            (reset! state nil)
            (answer-fn question))
          (let [question (question-fn)]
            (reset! state {:question question})
            question))))))


(def color-player
  "guess the color of the square"
  (puzzle-player {:question-fn rand-square
                  :answer-fn square-color}))

(def file-arithmetic-player
  "do arithmetic on files"
  (puzzle-player {:question-fn file-arithmetic-problem
                  :answer-fn   file-arithmetic-solution}))

(def diagonal-other-end-player
  "what square is on the other side of the diagonal"
  (puzzle-player {:question-fn rand-diagonal-end
                  :answer-fn   diagonal-other-end}))

(def pos-diagonal-for-square-player
  "which positive diagonal is this square a part of"
  (puzzle-player {:question-fn rand-square
                  :answer-fn positive-diagonal-for-square}))

(def neg-diagonal-for-square-player
  "which negative diagonal is this square a part of"
  (puzzle-player {:question-fn rand-square
                  :answer-fn negative-diagonal-for-square}))

(def diagonal-arithmetic-player
  "do arithmetic on a diagonal"
  (puzzle-player {:question-fn diagonal-arithmetic-problem
                  :answer-fn diagonal-arithmetic-solution}))

(def knights-moves-player
  "which knights moves are legal from this square"
  (puzzle-player {:question-fn rand-square
                  :answer-fn   knights-moves-for-square}))

;;; TODO: create a game that lists all squares a given piece can be on to attack
;;; both squares, e.g. Queen on d4 attacks d1 and g7 - this is what generates
;;; tactical "vision"

(comment
  (color-player)
  (file-arithmetic-player)
  (diagonal-other-end-player)
  (pos-diagonal-for-square-player)
  (neg-diagonal-for-square-player)
  (diagonal-arithmetic-player)
  (knights-moves-player)
  (rand-piece-and-path))
