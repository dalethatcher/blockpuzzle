(ns blockpuzzle)

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
    (if (contains? nil moved-state)
      []
      (undirectionise direction moved-state)))
)

(defn find-possible-children [state]
  []
)

(defn find-solution [start end]
  []
)
