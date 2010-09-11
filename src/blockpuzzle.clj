(ns blockpuzzle)

(defn pieces [state]
  (set (filter #(not (= 0 %)) (flatten state)))
)

(defn cut-in-direction-of-move [direction state]
  (let [switched (if (or (= :up direction) (= :down direction))
                   (apply map list state)
                   state)
        directionised (if (or (= :down direction) (= :right direction))
                        (map reverse switched)
                        switched)]
    directionised)
)

(defn move [piece direction state]
  []
)

(defn find-possible-children [state]
  []
)

(defn find-solution [start end]
  []
)
