(ns poker)

; Helpers

(defn card-order [c]
  (let [r (first c)]
    (cond
      (= r \A) 14
      (= r \K) 13
      (= r \Q) 12
      (= r \J) 11
      (= r \T) 10
      :else (Character/getNumericValue r))))

(defn sort-cards [hand]
  (sort-by card-order hand))

; 1.

(defn suit [card]
  (str(second(str card))))

; 2.

(defn convert_rank [card]
  (cond
    (Character/isDigit card)  (Integer/valueOf (str card))
    (= card \T) 10
    (= card \J) 11
    (= card \Q) 12
    (= card \K) 13
    (= card \A) 14))

(defn rank [card]
  (convert_rank (first (str card))))

; 3.

(defn pair? [hand]
  (boolean
    (some
      (fn[x] (= 2 (val x)))
      (frequencies
        (map
          (fn[x] (first x))
          hand)))))

; 4.

(defn three-of-a-kind? [hand]
  (boolean
    (some
      (fn[x] (= 3 (val x)))
      (frequencies
        (map
          (fn[x] (first x))
          hand)))))

; 5.

(defn four-of-a-kind? [hand]
  (boolean
    (some
      (fn[x] (= 4 (val x)))
      (frequencies
        (map
          (fn[x] (first x))
          hand)))))

; 6.

(defn flush? [hand]
  (let [first_suit (second (first hand))
        suits (map second hand)]
    (every? #(= first_suit %) suits)))

; 7.

(defn full-house? [hand]
  (let [freq (frequencies (map first hand))]
    (and
      (= 2 (count freq))
      (some #(= 3 (val %)) freq)
      (some #(= 2 (val %)) freq))))

; 8.

(defn two-pairs? [hand]
  (let [freq (frequencies (map first hand))
        pairs (filter #(= 2 (val %)) freq)]
    (and
      (= 3 (count freq))
      (= 2 (count pairs)))))

; 9.

(defn card-to-num [card]
  (let [r (first (str card))]
    (cond
      (Character/isDigit r) (Character/getNumericValue r)
      (= r \T) 10
      (= r \J) 11
      (= r \Q) 12
      (= r \K) 13
      (= r \A) 14)))

(defn straight? [hand]
  (boolean
    (let [freq (frequencies (map first hand))
          original (filter #(= 1 (val %)) freq)
          values (map card-to-num hand)
          sorted-values (sort values)
          has-ace (some #(= 14 %) values)
          ace-low-values (if has-ace
                           (conj (disj (set values) 14) 1)
                           (set values))
          regular-straight? (= (- (last sorted-values) (first sorted-values)) 4)
          ace-low-straight? (and has-ace
                                 (= ace-low-values #{1 2 3 4 5}))]
      (and
        (= 5 (count original))
        (or regular-straight?
            ace-low-straight?)))))

; 10.

(defn straight-flush? [hand]
  (cond
    (and (straight? hand) (flush? hand)) true
    :else false))

; 11.

(defn value [hand]
  (cond
    (straight-flush? hand) 8
    (four-of-a-kind? hand) 7
    (full-house? hand) 6
    (flush? hand) 5
    (straight? hand) 4
    (three-of-a-kind? hand) 3
    (two-pairs? hand) 2
    (pair? hand) 1
    :else 0))

; 12.

(defn kickers [hand]
  (let [values (map rank hand)
        sorted-values (sort > values)
        freq (frequencies values)
        ranks-by-count (group-by second (map vec freq))
        hand-value (value hand)]  ; Calculate value once
    (cond
      ; For straight flush (8), straight (4), or flush (5)
      (or (= 8 hand-value)
          (= 4 hand-value)
          (= 5 hand-value))
      sorted-values

      ; For four of a kind (7)
      (= 7 hand-value)
      [(ffirst (get ranks-by-count 4))
       (ffirst (get ranks-by-count 1))]

      ; For full house (6)
      (= 6 hand-value)
      [(ffirst (get ranks-by-count 3))
       (ffirst (get ranks-by-count 2))]

      ; For three of a kind (3)
      (= 3 hand-value)
      (concat
        [(ffirst (get ranks-by-count 3))]
        (sort > (map first (get ranks-by-count 1))))

      ; For two pairs (2)
      (= 2 hand-value)
      (concat
        (sort > (map first (get ranks-by-count 2)))
        [(ffirst (get ranks-by-count 1))])

      ; For one pair (1)
      (= 1 hand-value)
      (concat
        [(ffirst (get ranks-by-count 2))]
        (sort > (map first (get ranks-by-count 1))))

      ; For high card (0)
      :else
      sorted-values)))

; 13.

(defn higher-kicker? [kicker1 kicker2]
  (cond
    (or (empty? kicker1) (empty? kicker2)) false
    (> (first kicker1) (first kicker2)) true
    (< (first kicker1) (first kicker2)) false
    :else (higher-kicker? (rest kicker1) (rest kicker2))
    )
  )

; 14.
(defn beats? [hand1 hand2]
  (let [value1 (value hand1)
        value2 (value hand2)]
    (cond
      (> value1 value2) true
      (< value1 value2) nil
      :else (higher-kicker? (kickers hand1) (kickers hand2)))))

; 15.

(defn tied? [hand1 hand2]
  (let [value1 (value hand1)
        value2 (value hand2)]
    (and (= value1 value2)
         (nil? (higher-kicker? (kickers hand1) (kickers hand2))))))

(defn winning-hand [hands]
  (when (seq hands)
    (cond
      (= 1 (count hands)) (first hands)
      (beats? (first hands) (second hands))
      (winning-hand (cons (first hands) (rest (rest hands))))
      (tied? (first hands) (second hands))
      (let [tied-hands (filter #(tied? (first hands) %) hands)]
        (if (> (count tied-hands) 1)
          tied-hands
          (winning-hand (rest hands))))
      :else (winning-hand (rest hands)))))

