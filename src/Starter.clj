(ns Starter)

; Auditoriska 1. ;

(* (first '(1, 2, 3, 4, 5)) (first (rest (rest '(1,2,3,4,5)))) (first (rest (rest (rest (rest '(1,2,3,4,5)))))))

(def x '(A B C))
(def y '(1 2 3))
(concat x y)
(reverse x)
(last x)

(defn palindrom [l]
  (concat l (rest (reverse l)))
  )

(palindrom '(1 2 3 4))

; Auditoriska 2. ;

(defn R2 [N1 N2]
  (Math/sqrt
    (+ (Math/pow (- (first N1) (first N2)) 2)
       (Math/pow (- (last N1) (last N2)) 2))))

(R2 '(10 15) '(4 8))

(defn Rectangle [x y]
  (print "The perimeter of the rectange will be:")
  (+ (* x 2) (* y 2))
  (print "While the area will be:")
  (* x y)
  )

(Rectangle '5 '6)

(defn nazad [List]
  (concat (reverse List) (reverse List))
  )

(nazad '(a b c))

(defn snoc [x lst]
  (concat lst (list x)))

(snoc 9 '(1 2 3 4 5 6 7 8))

(defn rotator [List]
  (cons (first (reverse List)) (reverse (rest (reverse List))))
  )

(rotator '(1 2 3 4 5 6 7))

(defn dane [first second third]
  (list (symbol? first) (number? second) (list? third))
  )

(dane '(a b c d) 12 '(a b c))

(defn pald [List]
  (cond
    (= List (reverse List)) "Yes it is a palindrome!"
    :else "No it isn't a palindrome"))

(pald '(a b b a))

(defn lastlist [entry]
  (cond
    (list? entry) (last entry)
    (symbol? entry) (entry)
    :else nil
    )
  )

(lastlist ())
(lastlist '(a b))
(lastlist '(a) )

(defn make_list [entry]
  (cond
    (symbol? entry) (list entry)
    (list? entry) entry
    (empty? entry) nil
    )
  )

(make_list '(a b c))
(make_list 'a)

(defn delenje [x y]
  (if
    (and (number? x) (number? y) (not= y 0)) (/ x y))
  )

> (delenje 6 3)

; Auditoriska 3. ;

(defn suma [a]
  (if (== a 0) 0 (+ a (suma (- a 1))))
  )


(suma 3)

(defn stepen
  ([m n] (stepen m n 1))
  ([m n pom]
   (if (== n 0) pom (stepen m (- n 1) (* pom m)))
   )
  )

(stepen 3 2)

(defn sum_stepen
  ([m n] (sum_stepen m n 0))
  ([m n pom]
   (if (== m 0) pom (sum_stepen (- m 1) n (+ pom (stepen m n))))
   )
  )

(sum_stepen 3 2)

(defn listsum
  ([list] (listsum list 0))
  ([list, acc] (if (empty? list) acc (listsum (rest list) (+ (first list) acc))))
  )

(listsum '[1 2 3 4])

(defn append1 [a b]
  (if (empty? b) a (append1 (concat a [(first b)]) (rest b)))
  )

(append1 '(a b) '(1 2))

(defn namali [a b agg]
  (if (empty? a)
    [b agg]
    (namali (rest a)
            (concat b [(/ (first a) 2)])
            (+ (first a) agg)))
  )

(namali '[2, 4, 6, 8] '[] '0)

; Auditoriska 4. ;

(defn brpodlisti
  ([list] (brpodlisti list 0))
  ([list acc]
   (if (empty? list)
     acc
     (if (sequential? (first list))
       (brpodlisti (rest list)
                   (brpodlisti (first list)
                               (+ acc 1)))
       (brpodlisti (rest list) acc)))))

(brpodlisti '((a) 3 4))
(brpodlisti '(a (2(3)) (4)))

(defn broevi[lista]
  (cond
    (empty? lista) nil
    (number? (first lista))(cons (first lista) (broevi (rest lista)))
    (list? (first lista)) (concat (broevi (first lista))(broevi (rest lista)))
    :else (broevi (rest lista))
    )
  )

(broevi '(3 (1 2) 4 c))

(defn dodadi[a lista]
  (cond
    (empty? lista) nil
    :else (cons (cons a (first lista))(dodadi a (rest lista)))
    )
  )


(dodadi 'a '((1 2 3) (a b c)))