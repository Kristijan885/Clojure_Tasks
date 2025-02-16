(ns domasna4)

;; Група 1

; (а)

(defn atomic [a]
  (if (coll? a) false true)
  )

; (б)

(defn member [x a]
  (cond
    (empty? a) false
    (= x (first a)) true
    :else (member x (rest a))))

; (в)

(defn my-count [a]
  (cond
    (empty? a) 0
    (list? (first a)) (my-count (rest a))
    (or (number? (first a)) (symbol? (first a))) (+ 1 (my-count (rest a)))
    :else (my-count (rest a))
    ))

; (г)

(defn append [a1 a2]
  (cond
    (empty? a1) a2
    :else (cons (first a1) (append (rest a1) a2))))


; (д)

(defn zip
  ([ls1 ls2] (zip ls1 ls2 []))
  ([ls1 ls2 final]
   (cond
     (or (empty? ls1) (empty? ls2)) final
     :else (zip (rest ls1) (rest ls2)
                ( conj final [(first ls1) (first ls2)])))))

; (ѓ)

(defn lookup [key list-of-pairs]
  (cond
    (empty? list-of-pairs) nil
    (= key (first (first list-of-pairs))) (second (first list-of-pairs))
    :else (lookup key (rest list-of-pairs))))

; (е)

(defn my-merge [lst1 lst2]
  (cond
    (empty? lst1) lst2
    (empty? lst2) lst1
    (< (first lst1) (first lst2)) (cons (first lst1) (my-merge (rest lst1) lst2))
    :else (cons (first lst2) (my-merge lst1 (rest lst2)))
    )
  )

; (ж)

(defn count-all [lst]
  (cond
    (empty? lst) 0
    (list? (first lst)) (+ (count-all (first lst)) (count-all (rest lst)))
    :else (+ 1 (count-all (rest lst)))
    ))

; (з)

(defn my-drop [n lst]
  (cond
    (== n 0) lst
    :else (my-drop (- n 1) (rest lst))))

; (ѕ)

(defn my-take [n lst]
  (cond
    (== n 0) '()
    (empty? lst) '()
    :else (cons (first lst) (my-take (- n 1) (rest lst)))
    )
  )

; (и)

(defn my-reverse
  ([lst] (my-reverse lst '()))
  ([lst return]
   (cond
     (empty? lst) return
     :else (my-reverse (rest lst) (cons (first lst) return)))))

; (ј)


(defn remove-duplicates
  ([lst] (remove-duplicates lst '()))
  ([lst return]
   (cond
     (empty? lst) return
     (some #(= % (first lst)) return) (remove-duplicates (rest lst) return)
     :else (remove-duplicates (rest lst) (concat return (list (first lst)))))))

; (к)

(defn my-flatten [lst]
  (cond
    (empty? lst) '()
    :else (concat (first lst) (my-flatten (rest lst)))))

;; Група 2

; (а)

(defn has-five? [n]
  (some #(= \5 %) (seq (str n))))

(defn buzz [list]
  (map (fn [n]
         (if (or (zero? (mod n 5)) (has-five? n)) :buzz n)
         ) list )
  )

; (б)

(defn create_list [n]
  (cond
    (= n 0) '()
    (or (= n 1) (= n 2)) (create_list (- n 1))
    :else (cons (- n 1) (create_list (- n 1)))))

(defn divisible-by? [n]
  #(and (zero? (mod n %)) (not (= n %))))

(defn divisors-of [n]
  (filter (divisible-by? n) (reverse( create_list (+ n 1)))))

(divisors-of '12)

; (в)

(defn compare-lengths [lst1 lst2]
  (if (>= (count lst1) (count lst2))
    lst1
    lst2))

(defn longest [list-of-lists]
  (reduce compare-lengths list-of-lists))


;; Група 3

; (а)

(defn my-map [f lst]
  (cond
    (empty? lst) '()
    :else (cons (f (first lst)) (my-map f (rest lst)))
    )
  )

; (б)

(defn my-filter [pred lst]
  (cond
    (empty? lst) '()
    ( pred (first lst)) (cons (first lst) (my-filter pred (rest lst)))
    :else (my-filter pred (rest lst))
    )
  )

; (в)

(defn my-reduce [f value? lst]
  (cond
    (empty? lst) value?
    (nil? value?) (my-reduce f (f (first lst) (first (rest lst))) (rest (rest lst)))
    :else (my-reduce f (f value? (first lst)) (rest lst))
    )
  )

; (г)

(defn my-flat-map [f lst]
  (if (empty? lst) '()
                   (if (list? (first lst))
                     (concat (my-flat-map f (first lst))
                             (my-flat-map f (rest lst)))
                     (concat (f (first lst))
                             (my-flat-map f (rest lst))))))