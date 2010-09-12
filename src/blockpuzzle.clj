(ns blockpuzzle
  (:require [clojure [zip :as zip]])
  (:use clojure.set))

(defn pieces [state]
  (set (filter #(not (= 0 %)) (flatten state)))
)

(defn directionise [direction state]
  (let [switched (if (or (= :up direction) (= :down direction))
                   (apply map list state)
                   state)
        directionised (if (or (= :down direction) (= :right direction))
                        (map reverse switched)
                        switched)]
    directionised)
)

(defn move-in-list [piece-identifier segment]
  (let [[before-piece remainder] (split-with #(not (= piece-identifier %)) segment)]
    (cond
      (empty? remainder) segment
      (not (= 0 (last before-piece))) nil
      true (let [before-space (butlast before-piece)
                 [piece after-piece] (split-with #(= piece-identifier %) remainder)]
             (concat before-space piece [0] after-piece))
    ))
)

(defn undirectionise [direction state]
  (let [reordered (if (or (= :down direction) (= :right direction))
                    (map reverse state)
                    state)
        reoriented (if (or (= :down direction) (= :up direction))
                     (apply map list reordered)
                     reordered)]
    reoriented)
)

(defn move [piece direction state]
  (let [directionised-state (directionise direction state)
        moved-state (map #(move-in-list piece %) directionised-state)]
    (if (some nil? moved-state)
      []
      [(undirectionise direction moved-state)]))
)

(defn find-possible-children [state]
  (let [piece-identifiers (pieces state)
        with-directions #(list [% :up] [% :down] [% :left] [% :right])
        test-pairs (mapcat with-directions piece-identifiers)]
    (mapcat (fn [[piece-identifier direction]] (move piece-identifier direction state))
         test-pairs))
)

(defn end? [end-state state]
  (let [flat-state (flatten state)
        flat-end (flatten end-state)
        state-pairs (map list flat-state flat-end)]
    state-pairs
    (empty? (filter (fn [[state-item end-item]]
                      (not (or (= 0 end-item)
                               (= state-item end-item))))
                    state-pairs))
  )
)

(defn find-solution [start end]
  (loop [search-lines [[start]]
         known-states #{start}]
    (if (empty? search-lines)
      []
      (let [current-search (first search-lines)
            future-searches (rest search-lines)
            current-state (last current-search)
            previous-states (butlast current-search)
            possible-children (find-possible-children current-state)
            unique-children (filter #(not (known-states %)) possible-children)
            new-searches (map #(concat current-search [%]) unique-children)
            new-known-states (reduce #(union %1 #{%2}) known-states unique-children)]
        (if (contains? new-known-states end)
          (concat current-search [end])
          (recur new-searches new-known-states))
      )
    )
  )
)
