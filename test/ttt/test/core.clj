(ns ttt.test.core
  (:use [ttt.core])
  (:use [clojure.test]))

(deftest current-state-test
  (is (nil? (current-state new-game)))
  (is (= :x (current-state [[:x nil :o] [:o :x nil] [nil nil :x]]))))
