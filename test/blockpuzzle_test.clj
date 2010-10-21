(ns blockpuzzle-test
  (:use [blockpuzzle] :reload)
  (:use [clojure.test])
  (:use [clojure.contrib.trace]))

(def *find-solution*)

(deftest pieces-test
  (is (= #{1 3 5} (pieces [[5 0 0]
                           [0 3 0]
                           [0 0 1]])))
)

(deftest directionise-test
  (is (= [[1 3]
          [2 4]]
         (directionise :up [[1 2]
                            [3 4]])))
  (is (= [[3 1]
          [4 2]]
         (directionise :down [[1 2]
                              [3 4]])))
  (is (= [[1 2]
          [3 4]]
         (directionise :left [[1 2]
                              [3 4]])))
  (is (= [[2 1]
          [4 3]]
         (directionise :right [[1 2]
                               [3 4]])))
)

(deftest move-in-list-test
  (is (= [1 0 2] (move-in-list 1 [0 1 2])))
  (is (= nil (move-in-list 1 [2 1 0])))
  (is (= [0 1 2] (move-in-list 3 [0 1 2])))
  (is (= [1 1 0] (move-in-list 1 [0 1 1])))
)

(deftest undirectionise-test
  (is (= [[1 2]
          [3 4]]
         (undirectionise :up [[1 3]
                              [2 4]])))
  (is (= [[1 2]
          [3 4]]
         (undirectionise :down [[3 1]
                                [4 2]])))
  (is (= [[1 2]
          [3 4]]
         (undirectionise :left [[1 2]
                                [3 4]])))
  (is (= [[1 2]
          [3 4]]
         (undirectionise :right [[2 1]
                                 [4 3]])))
)

(deftest directionise-undirectionise-test
  (let [state [[1 2] [3 4]]]
    (is (= state (undirectionise :up (directionise :up state))))
    (is (= state (undirectionise :down (directionise :down state))))
    (is (= state (undirectionise :left (directionise :left state))))
    (is (= state (undirectionise :right (directionise :right state))))
))

(deftest move-test
  (is (= [[[0 1 0]
           [0 0 0]
           [0 0 0]]]
         (move 1 :up [[0 0 0]
                      [0 1 0]
                      [0 0 0]])))
  (is (= [[[0 0 0]
           [0 0 0]
           [0 1 0]]]
         (move 1 :down [[0 0 0]
                        [0 1 0]
                        [0 0 0]])))
  (is (= [[[0 0 0]
           [1 0 0]
           [0 0 0]]]
         (move 1 :left [[0 0 0]
                        [0 1 0]
                        [0 0 0]])))
  (is (= [[[0 0 0]
           [0 0 1]
           [0 0 0]]]
         (move 1 :right [[0 0 0]
                         [0 1 0]
                         [0 0 0]])))
  (is (= [] (move 1 :up [[0 0 2]
                         [0 1 1]])))
  (is (= [] (move 1 :up [[0 2 0]
                         [0 1 0]])))
  (is (= [] (move 1 :up [[0 1 0]
                         [0 0 0]])))
)

(deftest find-possible-children-test
  (let [children (find-possible-children [[0 0 0]
                                          [0 1 0]
                                          [0 0 0]])]
    (is (= 4 (count children)))
    (is (some #(= [[0 1 0]
                   [0 0 0]
                   [0 0 0]] %) children))
    (is (some #(= [[0 0 0]
                   [0 0 1]
                   [0 0 0]] %) children))
    (is (some #(= [[0 0 0]
                   [0 0 0]
                   [0 1 0]] %) children))
    (is (some #(= [[0 0 0]
                   [1 0 0]
                   [0 0 0]] %) children))
  )
  (is (= [] (find-possible-children [[1 2] [3 4]])))
)

(deftest end?-test
  (is (= true (end? [[1 1 0]
                     [0 0 0]
                     [0 0 0]]
                    [[1 1 2]
                     [0 0 3]
                     [4 5 6]])))
  (is (= false (end? [[1 1 0]
                      [0 0 0]
                      [0 0 0]]
                     [[0 0 2]
                      [1 1 3]
                      [4 5 6]])))
)

(deftest format-state-test
  (is (= "      1 2\n    3 4 5\n    6 7 8"
         (format-state [[0 1 2] [3 4 5] [6 7 8]])))
)

(deftest find-identical-pieces-test
  (is (= #{ #{1 2} #{3 4} #{5} }
	(find-identical-pieces [[1 3 0 2 5 5]
				[4 3 3 0 5 5]
				[4 4 0 0 0 0]]))
      )
  )

(deftest piece-silhouette-test
  (is (= [[0 1 0]
	  [0 1 1]]
	   (piece-silhouette [[1 2 3]
			      [1 2 2]] 2)))
  )

(deftest trim-blanks-test
  (is (= [[1]]
	   (trim-blanks [[0 0 0]
			 [0 1 0]
			 [0 0 0]])))
  )

(deftest radix-map-test
  (let [m (radix-map #{#{1 2} #{3}})]
    (is (= (m 0) 0))
    (is (= (m 1) (m 2)))
    (is (not (= (m 2) (m 3))))
    )
  )

(deftest state-to-identifier-test
  (let [rmap {0 0 1 1 2 1 3 2}
	state [[0 1 2] [3 3 0]]]
    (is (= 4r011220
	   (state-to-identifier rmap state)))
    )
  )

(deftest find-solution-one-column-block-story
  (is (= [[[0] [0] [1]] [[0] [1] [0]] [[1] [0] [0]]]
         (*find-solution* [[0] [0] [1]] [[1] [0] [0]])
  ))
)

(deftest find-solution-one-row-story
  (is (= [[[0 0 1]] [[0 1 0]] [[1 0 0]]]
         (*find-solution* [[0 0 1]] [[1 0 0]])
  ))
)

(deftest find-solution-none-possible-story
  (is (= []
         (*find-solution* [[0 2 1]] [[1 2 0]])
  ))
)

(deftest find-solution-multi-block-story
  (is (= [[[0 4 3]
           [2 1 1]]
          [[2 4 3]
           [0 1 1]]
          [[2 4 3]
           [1 1 0]]]
         (*find-solution* [[0 4 3]
                           [2 1 1]]
                          [[0 0 0]
                           [1 1 0]])))
)

(deftest find-solution-large-block-story
  (is (not (empty?
    (*find-solution* [[1  1  7  7 ]
                      [1  1  0  0 ]
                      [9  9  0  0 ]]
                     [[0  0  0  0 ]
                      [1  1  0  0 ]
                      [1  1  0  0 ]])
  )))
)

(deftest find-solution-stories
  (find-solution-one-column-block-story)
  (find-solution-one-row-story)
  (find-solution-none-possible-story)
  (find-solution-multi-block-story)
  (find-solution-large-block-story)
)

(defn test-ns-hook []
  (pieces-test)
  (directionise-test)
  (move-in-list-test)
  (undirectionise-test)
  (directionise-undirectionise-test)
  (move-test)
  (find-possible-children-test)
  (end?-test)
  (format-state-test)
  (find-identical-pieces-test)
  (piece-silhouette-test)
  (trim-blanks-test)
  (radix-map-test)
  (state-to-identifier-test)
  (binding [*find-solution* find-solution-breadth] (find-solution-stories))
  (binding [*find-solution* (partial find-solution-depth 5)] (find-solution-stories))
  (binding [*find-solution* (partial find-solution-scored (fn [_] 1))]
    (find-solution-stories))
)