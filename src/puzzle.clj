(ns puzzle
  (:gen-class)
  (:use blockpuzzle))

(defn print-solution []
  (do
    (println "Starting solution search")
    (println
      (let [solution (find-solution-depth
                        100
                        [[2  2  3  3  5]
                         [1  1  6  7  0]
                         [1  1  6  8  0]
                         [9  9  10 10 11]]
                        [[0  0  0  0  0]
                         [0  0  0  1  1]
                         [0  0  0  1  1]
                         [0  0  0  0  0]])]
        (if (empty? solution)
          "No solution found."
          (format-states solution)
        )
      )
    )
  )
)

(defn -main [& args]
  (print-solution)
)
