(ns puzzle
  (:use blockpuzzle))

(defn print-solution []
  (println
    (format-states
      (find-solution
        [[2  2  3  3  5]
         [1  1  6  7  0]
         [1  1  6  8  0]
         [9  9  10 10 11]]
        [[0  0  0  0  0]
         [0  0  0  1  1]
         [0  0  0  1  1]
         [0  0  0  0  0]])))
)
