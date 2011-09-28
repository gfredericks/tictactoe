(ns ttt.core
  (:require [clojure.string :as string])
  (:use [slingshot.core :only [try+ throw+]])
  (:gen-class))

(defn map3 [f] (map f (range 3)))

(def new-game (repeat 3 (repeat 3 nil)))

;; Canonization

(defn col
  [g i]
  (map #(nth % i) g))

(def row nth)

(defn transpose
  [g]
  (map3 #(col g %)))

(def rotate (comp reverse transpose))

(defn equivalents
  [g]
  (let [rotates (take 4 (iterate rotate g))]
    (concat rotates (map transpose rotates))))

(defn vecs [g] (vec (map vec g)))

(defn canonize
  [g]
  (->> (equivalents g)
       (map vecs)
       (sort)
       (first)))

;; Moving
;; Moves are represented as
;; [[1 2 3]
;;  [4 5 6]
;;  [7 8 9]]

(def all-possible-moves (set (range 1 10)))

(defn row-for-move [move] (quot (dec move) 3))
(defn col-for-move [move] (rem  (dec move) 3))

(defn el-at
  [g move]
  (-> g
    (row (row-for-move move))
    (nth (col-for-move move))))

(defn set-el
  [g move val]
  {:pre [(all-possible-moves move) (contains? #{:x :o nil} val)]}
  (let [g (vecs g)]
    (->> val
      (assoc (row g (row-for-move move)) (col-for-move move))
      (assoc g (row-for-move move)))))

(defn available-moves
  [g]
  (filter #(nil? (el-at g %)) all-possible-moves))

;; Evaluation

(defn all-triples
  [g]
  (concat
    ; g itself is a list of its rows
    g
    ; its transpose is a list of its columns
    (transpose g)
    ; both diagonals
    [(map3 #(nth (row g %) %)) (map3 #(nth (row g (- 2 %)) %))]))

(defn current-state
  "Is this game already won or still in progress.
  Returns :x, :o, or nil."
  [g]
  (loop [[trip & trips] (all-triples g)]
    (if trip
      (let [zs (distinct trip)]
        (cond
          (= zs [:x]) :x
          (= zs [:o]) :o
          :else (recur trips))))))

(def opposite {:x :o, :o :x})

(defn desirability
  [to-move outcome]
  (if (= :x to-move)
    ({:x 1 nil 2 :o 3} outcome)
    ({:o 1 nil 2 :x 3} outcome)))

; Also returns :x, :o, or nil
(def ultimate-state*
  (memoize
    (fn [g to-move]
      (if-let [game-over (current-state g)]
        game-over
        (let [moves (available-moves g),
              moves-with-outcomes
                (for [move moves]
                  (let [game-after-move (set-el g move to-move)]
                    [(ultimate-state* (canonize game-after-move) (opposite to-move))
                     move])),
              with-desirability
                (for [[outcome move] moves-with-outcomes]
                  [(desirability to-move outcome) outcome move]),
              ; This could be optimized by not sorting but rather walking through the
              ; lazy seq and returning early if a winning move is found
              sorted
                (sort-by first with-desirability),
              best-outcome (-> sorted first second)]
          best-outcome)))))

(defn ultimate-state
  [g to-move]
  (ultimate-state* (canonize g) to-move))

(defn best-moves
  "Returns all moves that don't make the position any worse."
  [g to-move]
  (let [moves (available-moves g),
        current-outcome (ultimate-state g to-move)]
    (filter #(= current-outcome (ultimate-state (set-el g % to-move) (opposite to-move))) moves)))

(def choose-move (comp rand-nth best-moves))

(defn make-move
  [g to-move]
  (set-el g (choose-move g to-move) to-move))

;; UI

(defn get-input
  []
  (println "Please input a move from 1 to 9 (as on a phone keypad):")
  (loop []
    (print "$ ")
    (.flush *out*)
    (let [s (.readLine *in*)]
      (cond
        (re-find #"^q" s)
          (throw+ :quit)
        (re-matches #"\d" s)
          (new Integer s)
        :else
          (do
            (println "Bad input! Expected a single digit or \"quit\".")
            (recur))))))

(def print-hr (partial println (apply str (repeat 35 "-"))))

(defn print-game
  [g]
  (print "\n")
  (println
    (string/join "\n-----\n"
      (for [row g]
        (string/join "|" (for [el row] (name (or el " "))))))
    "\n"))

(defn user-turn
  [g to-move]
  (print-hr)
  (println "Your turn!")
  (print-game g)
  (loop [move (get-input)]
    (if ((set (available-moves g)) move)
      (set-el g move to-move)
      (do
        (println "That move isn't available!")
        (recur (get-input))))))

(defn bot-turn
  [g to-move]
  (let [move (choose-move g to-move)]
    (println "Bot places a" (name to-move) "in square" move)
    (set-el g move to-move)))

(defn play-game
  [player-starts?]
  (print "\n")
  (print-hr)
  (println "  NEW GAME")
  (print-hr)
  (loop [g new-game, to-move :x, player-moves? player-starts?]
    (let [g (if player-moves? (user-turn g to-move) (bot-turn g to-move))]
      (if-let [res (current-state g)]
        (do
          (print-game g)
          (println (name res) "wins!"))
        (if (empty? (available-moves g))
          (do
            (print-game g)
            (println "Tie."))
          (recur g (opposite to-move) (not player-moves?)))))))

(defn -main
  []
  ; prime the memoizer in another thread to reduce first-move-lag
  (future (ultimate-state new-game :x))
  (try+
    (loop []
      (play-game (rand-nth [true false]))
      (Thread/sleep 2000)
      (recur))
    (catch #{:quit} _
      (println "Seeya"))))
