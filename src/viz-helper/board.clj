(ns viz-helper.board
  (:require [cemerick.url :as url]
            [clojure.java.browse :as browse]))

(def STARTING-BOARD
  {:piece-placement
   [[{:color :white
      :piece :rook}
    {:color :white
      :piece :knight}
    {:color :white
      :piece :bishop}
    {:color :white
      :piece :queen}
    {:color :white
      :piece :king}
    {:color :white
      :piece :bishop}
    {:color :white
      :piece :knight}
    {:color :white
     :piece :rook}]
    [{:color :white
      :piece :pawn}
    {:color :white
      :piece :pawn}
    {:color :white
      :piece :pawn}
    {:color :white
      :piece :pawn}
    {:color :white
      :piece :pawn}
    {:color :white
      :piece :pawn}
    {:color :white
      :piece :pawn}
    {:color :white
      :piece :pawn}]
    [nil nil nil nil nil nil nil nil]
    [nil nil nil nil nil nil nil nil]
    [nil nil nil nil nil nil nil nil]
    [nil nil nil nil nil nil nil nil]
    [{:color :black
      :piece :pawn}
    {:color :black
      :piece :pawn}
    {:color :black
      :piece :pawn}
    {:color :black
      :piece :pawn}
    {:color :black
      :piece :pawn}
    {:color :black
      :piece :pawn}
    {:color :black
      :piece :pawn}
    {:color :black
      :piece :pawn}]
    [{:color :black
      :piece :rook}
    {:color :black
      :piece :knight}
    {:color :black
      :piece :bishop}
    {:color :black
      :piece :queen}
    {:color :black
      :piece :king}
    {:color :black
      :piece :bishop}
    {:color :black
      :piece :knight}
    {:color :black
     :piece :rook}]]

   :active-color :white

   :castling-availability
   {:white {:kingside  true
            :queenside true}
    :black {:kingside  true
            :queenside true}}

   ;if any, the pawn that can be taken en-passant
   :en-passant-target nil

   ;moves since a capture or pawn advance
   :halfmove-clock 0

   ;number of full moves played (starting at 1)
   :fullmove-number 1})


(defn view-fen-on-lichess [fen]
  (->> fen
       url/url-encode
       (str "https://lichess.org/editor?fen=")
       browse/browse-url))
