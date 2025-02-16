(ns sudoku)

(defn inner_transform "Where the actual value substitution happens" [list]
  (cond
    (empty? list) '()
    (not= 0 (first list)) (cons [(first list)] (inner_transform (rest list)))
    :else (cons [1 2 3 4 5 6 7 8 9] (inner_transform (rest list)))
    )
  )



(defn transform "This one act's as an outer transformer calling the inner one"
  ([matrix] (transform inner_transform matrix))
  ([inner matrix]
   (cond
     (empty? matrix) '()
     (or (list? (first matrix))
         (vector? (first matrix))) (cons (inner (first matrix))
                                         (transform inner (rest matrix)))
     :else '()
     )
   )
  )

(defn row_function
  ([list] (row_function list (map first (filter #(= 1 (count %)) list))))
  ([list fixed-nums]
   (cond
     (empty? list) '()
     (= 1 (count (first list))) (cons (first list) (row_function (rest list) fixed-nums))
     :else (let [possible-nums (remove #(some #{%} fixed-nums) [1 2 3 4 5 6 7 8 9])]
             (cons (vec possible-nums) (row_function (rest list) fixed-nums))))))

(defn row_constraint
  ([matrix] (row_constraint row_function matrix))
  ([row_function matrix]
   (cond
     (empty? matrix) '()
     (sequential? (first matrix)) (cons (row_function (first matrix))
                                        (row_constraint row_function
                                                        (rest matrix)))
     :else '())))


(defn get_column [matrix n]
  (map #(nth % n) matrix))

(defn col_function
  ([col] (col_function col (map first (filter #(= 1 (count %)) col))))
  ([col fixed-nums]
   (cond
     (empty? col) '()
     :else (let [curr (first col)
                 curr-fixed (= 1 (count curr))
                 possible-nums (if curr-fixed
                                 curr
                                 (vec (remove #(some #{%} fixed-nums)
                                              (vec (first col)))))]
             (cons possible-nums (col_function (rest col) fixed-nums))))))

(defn col_constraint
  ([matrix] (col_constraint col_function matrix))
  ([col_function matrix]
   (if (empty? matrix)
     '()
     (let [width (count (first matrix))
           cols (map #(get_column matrix %) (range width))
           processed-cols (map col_function cols)]
       (apply map vector processed-cols)))))


(defn get_box [matrix row col]
  (let [box-row (- row (mod row 3))
        box-col (- col (mod col 3))]
    (vec (for [r (range box-row (+ box-row 3))
               c (range box-col (+ box-col 3))]
           (nth (nth matrix r) c)))))

(defn set_box [matrix row col new-values]
  (let [box-row (- row (mod row 3))
        box-col (- col (mod col 3))
        indices (for [r (range box-row (+ box-row 3))
                      c (range box-col (+ box-col 3))]
                  [r c])
        pairs (map vector indices new-values)
        matrix (if (seq? matrix) (vec matrix) matrix)
        matrix (map #(if (seq? %) (vec %) %) matrix)
        matrix (vec matrix)]
    (reduce (fn [m [pos val]]
              (assoc-in m pos val))
            matrix
            pairs)))

(defn box_function
  ([box] (box_function box (map first (filter #(= 1 (count %)) box))))
  ([box fixed-nums]
   (vec (map (fn [cell]
               (if (= 1 (count cell))
                 cell
                 (vec (remove #(some #{%} fixed-nums) (seq cell)))))
             box))))

(defn box_constraint
  ([matrix] (box_constraint box_function matrix))
  ([box_function matrix]
   (let [matrix (if (seq? matrix) (vec matrix) matrix)
         matrix (map #(if (seq? %) (vec %) %) matrix)
         matrix (vec matrix)]
     (loop [row 0
            col 0
            result matrix]
       (if (>= row 9)
         result
         (if (>= col 9)
           (recur (+ row 3) 0 result)
           (let [box (get_box result row col)
                 processed-box (box_function box)
                 new-result (set_box result row col processed-box)]
             (recur row (+ col 3) new-result))))))))

(defn get_final_possibilities [matrix]
  (box_constraint
    (col_constraint
      (row_constraint
        (transform matrix)))))

(get_final_possibilities   '[[5 3 0 0 7 0 0 0 0]
                             [6 0 0 1 9 5 0 0 0]
                             [0 9 8 0 0 0 0 6 0]
                             [8 0 0 0 6 0 0 0 3]
                             [4 0 0 8 0 3 0 0 1]
                             [7 0 0 0 2 0 0 0 6]
                             [0 6 0 0 0 0 2 8 0]
                             [0 0 0 4 1 9 0 0 5]
                             [0 0 0 0 8 0 0 7 9]])

(defn valid-move? [board row col value]
  (let [box-row (* 3 (quot row 3))
        box-col (* 3 (quot col 3))]
    (and
      (not-any? #(= [value] (nth (nth board row) %))
                (range 9))
      (not-any? #(= [value] (nth (nth board %) col))
                (range 9))
      (not-any? #(= [value] (nth (nth board (+ box-row (quot % 3)))
                                 (+ box-col (mod % 3))))
                (range 9)))))

(defn get-possibilities [cell board row col]
  (if (= 1 (count cell))
    [(first cell)]
    (filter #(valid-move? board row col %) (seq cell))))

(defn solve [board]
  (let [initial-board (get_final_possibilities board)]
    (loop [stack [[initial-board 0 0]]]
      (if (empty? stack)
        nil
        (let [[current row col] (first stack)]
          (cond
            (>= row 9) current  ; Solution found!
            (>= col 9) (recur (cons [current (inc row) 0]
                                    (rest stack)))
            (= 1 (count (nth (nth current row) col)))  ; Fixed cell
            (recur (cons [current row (inc col)]
                         (rest stack)))
            :else
            (let [possibilities (get-possibilities (nth (nth current row) col)
                                                   current row col)]
              (if (empty? possibilities)
                (recur (rest stack))  ; Backtrack
                (recur (concat
                         (for [val possibilities]
                           [(assoc-in current [row col] [val])
                            row (inc col)])
                         (rest stack)))))))))))



(defn simplify-solution [board]
  (mapv #(mapv (fn [cell] (first cell)) %) board))

(defn print-line []
  (println "------------------------------------------------"))

(defn get-cell-value [cell]
  (if (or (nil? cell) (= 0 cell))
    " "
    (str cell)))

(defn print-row [row]
  (print "|")
  (doseq [i (range 9)]
    (print " " (get-cell-value (nth row i)) " ")
    (when (= (mod (inc i) 3) 0)
      (print "|")))
  (println))

(defn visualize-board [board]
  (doseq [i (range 9)]
    (when (= (mod i 3) 0)
      (print-line))
    (print-row (nth board i)))
  (print-line))

(defn print-solution [input solution]
  (println "\nInput Board:")
  (visualize-board input)
  (println "\nSolution:")
  (visualize-board (simplify-solution solution)))

(defn solve-with-viz [board]
  (let [solution (solve board)]
    (print-solution board solution)
    solution))

(solve-with-viz   '[[5 3 0 0 7 0 0 0 0]
                             [6 0 0 1 9 5 0 0 0]
                             [0 9 8 0 0 0 0 6 0]
                             [8 0 0 0 6 0 0 0 3]
                             [4 0 0 8 0 3 0 0 1]
                             [7 0 0 0 2 0 0 0 6]
                             [0 6 0 0 0 0 2 8 0]
                             [0 0 0 4 1 9 0 0 5]
                             [0 0 0 0 8 0 0 7 9]])




