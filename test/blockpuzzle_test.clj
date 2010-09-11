(ns blockpuzzle-test
  (:use [blockpuzzle] :reload)
  (:use [clojure.test]))

(deftest pieces-test
  (is (= #{1 3 5} (pieces [[5 0 0]
                           [0 3 0]
                           [0 0 1]])))
)

(deftest cut-in-direction-of-move-test
  (is (= [[1 3]
          [2 4]]
         (cut-in-direction-of-move :up [[1 2]
                                        [3 4]])))
  (is (= [[3 1]
          [4 2]]
         (cut-in-direction-of-move :down [[1 2]
                                          [3 4]])))
  (is (= [[1 2]
          [3 4]]
         (cut-in-direction-of-move :left [[1 2]
                                          [3 4]])))
  (is (= [[2 1]
          [4 3]]
         (cut-in-direction-of-move :right [[1 2]
                                           [3 4]])))
)

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
  (is (contains? children [[0 1 0]
                           [0 0 0]
                           [0 0 0]]))
  (is (contains? children [[0 0 0]
                           [0 0 1]
                           [0 0 0]]))
  (is (contains? children [[0 0 0]
                           [1 0 0]
                           [0 0 0]]))
  (is (contains? children [[0 0 0]
                           [0 0 0]
                           [0 1 0]]))
)
)

(deftest find-solution-one-column-block-story
(is (= [[[0] [0] [1]] [[0] [1] [0]] [[1] [0] [0]]]
       (find-solution [[0] [0] [1]] [[1] [0] [0]])
))
)

(deftest find-solution-one-row-story
(is (= [[[0 0 1]] [[0 1 0]] [[1 0 0]]]
       (find-solution [[0 0 1]] [[1 0 0]])
))
)

(deftest find-solution-none-possible-story
(is (= []
       (find-solution [[0 2 1]] [[1 2 0]])
))
)
