(ns boja)

(defn rotate-colors
  [color-sequence]
  (if (empty? color-sequence)
    '()
    (let [reversed (reverse color-sequence)]
      (cons (first reversed) (reverse (rest reversed))))))


(defn unique-colors-in-column?
  "Checks if colors in corresponding positions are all different"
  [color-arrangements]
  (cond
    (empty? (first color-arrangements)) true
    :else
    (and
      (let [column-colors (map first color-arrangements)]
        (= (count column-colors) (count (distinct column-colors))))
      (unique-colors-in-column? (map rest color-arrangements)))))

(defn find-valid-arrangement
  "Recursively tries different color arrangements until a valid one is found"
  [remaining-rows valid-rows rotation-count]
  (cond
    (empty? remaining-rows) valid-rows
    (>= rotation-count (count (first remaining-rows))) nil
    (unique-colors-in-column? (cons (first remaining-rows) valid-rows))
    (or
      (find-valid-arrangement
        (rest remaining-rows)
        (cons (first remaining-rows) valid-rows)
        0)
      (find-valid-arrangement
        (cons (rotate-colors (first remaining-rows)) (rest remaining-rows))
        valid-rows
        (inc rotation-count)))
    :else
    (find-valid-arrangement
      (cons (rotate-colors (first remaining-rows)) (rest remaining-rows))
      valid-rows
      (inc rotation-count))))

(defn arrange-colors
  "Entry point function to find a valid color arrangement"
  [color-rows]
  (find-valid-arrangement color-rows '() 0))
