(ns blockpuzzle
  (:use clojure.set))

(defn pieces [state]
  (set (filter #(not (= 0 %)) (flatten state)))
  )

(defn transpose [state]
  (apply map list state))

(defn directionise [direction state]
  (let [switched (if (or (= :up direction) (= :down direction))
                   (transpose state)
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
      :else (let [before-space (butlast before-piece)
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

(defn format-state [state]
  (.replace
    (str "    "
      (reduce #(str %1 "\n    " %2)
        (map #(apply str (interpose " " %)) state)
      )
    )
  "0" " ")
)

(defn format-states [states]
  (reduce str "\n" (interpose "\n    ---------\n" (map format-state states)))
  )

(defn piece-silhouette [state piece]
  (letfn [(silhouette-row [row] (map #(if (= piece %) 1 0) row))]
    (map silhouette-row state)))

(defn empty-row? [row]
  (empty? (filter #(not (zero? %)) row)))

(defn remove-blank-rows [state]
  (filter #(not (empty-row? %)) state))

(defn trim-blanks [state]
  (let [blank-rows-removed (remove-blank-rows state)
	transposed (transpose blank-rows-removed)
	blank-columns-removed (remove-blank-rows transposed)]
    (transpose blank-columns-removed))
  )

(defn find-identical-pieces [state]
  (let [piece-identifiers (pieces state)
	silhouette-map (reduce #(assoc %1 %2 (trim-blanks (piece-silhouette state %2)))
			       {} piece-identifiers)
	silhouette-to-pieces (reduce (fn [m [piece silhouette]]
				       (assoc m silhouette (union (m silhouette)
								  #{piece})))
				     {} silhouette-map)]
    (set (vals silhouette-to-pieces)))
  )

(defn radix-map
  "Takes a set of sets of identical shaped pieces and produces a mapping
   from piece identifier to common radix for that shape."
  [identical-pieces-set]
  (let [radix-piece-pairs (map list (iterate inc 1) identical-pieces-set)
	exploded-pairs (map (fn [[i pieces]] (map list pieces (repeat i))) radix-piece-pairs)]
    (apply hash-map (concat [0 0] (flatten exploded-pairs))))
  )

(defn state-to-identifier [rmap state]
  (let [radix (count rmap)]
    (reduce #(+ (* radix %1) (rmap %2))
	    0
	    (flatten state)))
  )

(defn find-solution-breadth [start end]
  (let [state-id (partial state-to-identifier (radix-map (find-identical-pieces start)))]
    (loop [search-lines [[start]]
	   known-states #{(state-id start)}
	   depth 0]
      (cond
       (empty? search-lines)
       []
       (end? end (last (first search-lines)))
       (first search-lines)
       (< depth (count (first search-lines)))
       (do
	 (println "Starting search at depth:" (inc depth)
		  "number of search lines:" (count search-lines))
	 (recur search-lines known-states (inc depth)))
       :else
       (let [current-search (first search-lines)
	     future-searches (rest search-lines)
	     current-state (last current-search)
	     previous-states (butlast current-search)
	     possible-children (find-possible-children current-state)
	     unique-children (filter #(not (known-states (state-id %))) possible-children)
	     new-unique-searches (doall (map #(concat current-search [%])
					     unique-children))
	     new-search-lines (doall (concat future-searches new-unique-searches))
	     new-known-states (reduce #(union %1 #{(state-id %2)}) known-states unique-children)]
	 (recur new-search-lines new-known-states depth)
	 )
       )
      ))
  )

(defn find-solution-depth [max-depth start end]
  (loop [search-lines [[start]]
         search-line-states (set [start])
         moves-tried 0
         next-report-time (+ 30000 (System/currentTimeMillis))]
    (cond
      (empty? search-lines)
        (do
          (println)
          [])
      (end? end (last (first search-lines)))
        (first search-lines)
      (> (System/currentTimeMillis) next-report-time)
        (do
          (println "backlog:" (count search-lines) "moves-tried:" moves-tried
                   "current-leaf:")
          (println (format-state (last (first search-lines))))
          (recur search-lines search-line-states moves-tried
                 (+ 30000 (System/currentTimeMillis)))
        )
      (>= (count (first search-lines)) max-depth)
        (let [new-search-lines (rest search-lines)]
          (recur new-search-lines (set (first new-search-lines))
		 moves-tried next-report-time)
        )
      :else
        (let [current-search (first search-lines)
              future-searches (rest search-lines)
              current-state (last current-search)
              previous-states (butlast current-search)
              possible-children (find-possible-children current-state)
              unique-children (filter #(not (search-line-states %))
				      possible-children)
              new-unique-searches (map #(concat current-search [%]) unique-children)
              new-search-lines (concat new-unique-searches future-searches)
              new-search-line-states (union search-line-states
					    (set unique-children))]
            (recur new-search-lines new-search-line-states
                   (+ moves-tried (count unique-children)) next-report-time)
        )
    )
  )
)

(defstruct search :history :score :state)

(defn insert-new-search [search-lines new-search]
  (let [score (new-search :score)
        depth (count (new-search :history))
        higher-priority (fn [s] (or (> (s :score) score)
                                    (and (= (s :score score))
                                         (< (count (s :history)) depth))))
        [higher-scores lower-scores] (split-with higher-priority search-lines)]
    (concat higher-scores [new-search] lower-scores))
)

(defn find-solution-scored [score? start end]
  (loop [search-lines [(struct search [] (score? start) start)]
         known-states #{start}
         last-report-time (System/currentTimeMillis)]
    (cond
      (empty? search-lines)
        []
      (> (System/currentTimeMillis) (+ last-report-time 20000))
        (do
          (let [depths (map #(+ 1 (count (% :history))) search-lines)
                max-depth (reduce #(if (> %2 %1) %2 %1) 0 depths)
                best-state (format-state ((first search-lines) :state))]
            (println "Number of search lines:" (count search-lines)
                     "max depth:" max-depth
                     "number of known states:" (count known-states)
                     "best state:")
            (println best-state))
          (recur search-lines known-states (System/currentTimeMillis)))
      :else
          (let [current-search (first search-lines)
                future-searches (rest search-lines)
                current-state (current-search :state)
                possible-children (find-possible-children current-state)
                unique-children (filter #(not (known-states %)) possible-children)
                end-states (filter #(end? end %) unique-children)]
            (if (not (empty? end-states))
              (concat (current-search :history) [current-state] end-states)
              (let [new-history (doall (concat (current-search :history)
					       [current-state]))
                    new-searches (map #(struct search new-history (score? %) %)
				      unique-children)
                    new-search-lines (doall (reduce #(insert-new-search %1 %2)
                                             future-searches new-searches))
                    new-known-states (reduce #(union %1 #{%2})
					     known-states unique-children)]
                (recur new-search-lines new-known-states last-report-time))))
    )
  )
)
