(ns puzzle
  (:gen-class)
  (:use blockpuzzle))

(defn state-score [state]
  (let [[_ second-row third-row _] state
        at-start (and (= (first second-row) 1) (= (first third-row) 1))
        has-moved-score (if at-start 0 1)
        is-not-one #(not (= 1 %))
        row-contains-one? (fn [row] (some #(= 1 %) row))
        rows-with-one (filter row-contains-one? state)
        distances-from-left (map #(count (take-while is-not-one %)) rows-with-one)
        distance-from-start-score (apply max distances-from-left)]
    (+ has-moved-score distance-from-start-score))
)

(defn print-solution []
  (do
    (println "Starting solution search")
    (println
      (let [solution (find-solution-breadth
                       [[2  2  3  3  5]
                        [1  1  6  7  0]
                        [1  1  6  8  0]
                        [9  9  4  4  "a"]]
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
