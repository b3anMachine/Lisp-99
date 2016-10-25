
(ns Lisp99)


(defn p01Niave [x]
  (if (> (count x) 1) (p01Niave (rest x)) (first x)))

;to get the last boxed element of a list
(defn p01 [x]
  (if (empty? x) x
                 (if (empty? (rest x)) x
                                       (p01 (rest x)))))

;to get the last two boxed elements of a list
(defn p02 [x]
  (if (empty? x) x
                 (if (empty? (rest x)) x
                                       (if (empty? (rest (rest x))) x
                                                                    (p02 (rest x))))))

(if (and (= (p02 '(1 2 3)) '(2 3))
         (= (p02 '()) '())
         (= (p02 '(1)) '(1))
         (= (p02 '('())) '('())))
  (println "p02 tests pass"))

;to find the kth element in a list x
(defn p03 [x k]
  (if (empty? x) nil
                 (if (= k 1) (first x)
                             (p03 (rest x) (- k 1)))))

(if (and (= (p03 '(1 2 3) 2) 2)
         (= (p03 '(1 2 3) 4) nil)
         (= (p03 '(1 2 3) 1) 1)
         (= (p03 '() 2) nil))
  (println "p03 tests pass"))


;to find the number of elements of a list
(defn p04-help [x c]
  (if (empty? x) c
                 (p04-help (rest x) (+ c 1))))

(defn p04Niave [x]
  (p04-help x 0))

(defn p04 [x]
  (if (empty? x) 0
                 (+ 1 (p04 (rest x)))))

(if (and (= (p04 '(1 2 3)) 3)
         (= (p04 '(1 2 3 4)) 4)
         (= (p04 '()) 0)
         (= (p04 '(1)) 1))
  (println "p04 tests pass"))


;to reverse a list
(defn p05Niave [x]
  (if (empty? x) x
                 (concat (p05Niave (rest x)) (list (first x)))))

(defn p05Help [x r]
  (if (empty? x) r
                 (p05Help (rest x) (cons (first x) r))))

(defn p05 [x]
  (p05Help x '()))

(if (and (= (p05 '(1 2 3)) '(3 2 1))
         (= (p05 '(1 2 3 4)) '(4 3 2 1))
         (= (p05 '()) '())
         (= (p05 '(1)) '(1)))
  (println "p05 tests pass"))

;To find if a list is a palindrome
(defn p06 [x]
  (if (= x (p05 x)) true false))

(if (and (= (p06 '(1 2 3 1 2)) false)
         (= (p06 '(1 2 3 2 1)) true)
         (= (p06 '()) true)
         (= (p06 '(1)) true))
  (println "p06 tests pass"))


;To flatten a list
(defn p07help [x a]
  (if (empty? x) a
                 (if (list? (first x)) (p07help (rest x) (concat (p07help (first x) (list)) a))
                                       (p07help (rest x) (concat (list (first x)) a)))))

(defn p07 [x]
  (p05 (p07help x (list))))

(if (and (= (p07 (list 1 (list 2 (list 3)))) (list 1 2 3))
         (= (p07 (list 1 (list 2 3) 4)) (list 1 2 3 4))
         (= (p07 (list (list))) (list))
         (= (p07 (list(list(list 1)))) (list 1)))
  (println "p07 tests pass"))


;to eliminate consecutive duplicates in a list
(defn p08help [x a]
  (if (empty? x) (p05 a)
                 (if  (= (first x) (second x)) (p08help (rest x) a)
                                               (p08help (rest x) (cons (first x) a)))))

(defn p08 [x]
  (p08help x (list)))


(if (and (= (p08 '(1 2 3)) '(1 2 3))
         (= (p08 '(1 2 2 3)) '(1 2 3))
         (= (p08 '(1 2 2 3 4 4)) '(1 2 3 4))
         (= (p08 '()) '()))
  (println "p08 tests pass"))



;to pack consecutive duplicates in a list into sublists

(defn p09help [x f p]
  (if
    (and (empty? x) (empty? p)) f
                                (if (empty? p) (p09help (rest x) f (list (first x)))
                                               (if (= (first x) (first p)) (p09help (rest x) f (cons (first x) p))
                                                                           (p09help x (cons p f) '())))))


(defn p09 [x]
  (p05 (p09help x '() '())))

;other attempt:
(comment
  (defn p09 [x]
    (with-local-vars [fin '(), pack '()]

      (cond
        (empty? x) f
        (empty? pack) (var-set [pack '((first x))])
        (= (first x) (second x))
        (not (empty? x))
        (empty? x) fin
        (empty? (rest x)
                (= (first x) (second x)) 1)))
    )
  )

(if (and (= (p09 '(1 2 3)) (list '(1) '(2) '(3)))
         (= (p09 '(1 2 2 3)) (list '(1) '(2 2) '(3)))
         (= (p09 '(1 2 2 3 4 4 4 2)) (list '(1) '(2 2) '(3) '(4 4 4) '(2)))
         (= (p09 '()) '()))
  (println "p09 tests pass"))

 
;takes list proccessed by p09 and processes result for p10 
(defn p10help [x r]
  (cond
    (empty? x) r
    :else (p10help (rest x) (conj r (list (p04 (first x)) (first (first x))))) 
    )
  )

(defn p10 [x]
    (p05 (p10help (p09 x) '()))
  )


(if (and (= (p10 '(1 2 3)) (list '(1 1) '(1 2) '(1 3)))
         (= (p10 '(1 2 2 3)) (list '(1 1) '(2 2) '(1 3)))
         (= (p10 '(1 2 2 3 4 4 4 2)) (list '(1 1) '(2 2) '(1 3) '(3 4) '(1 2)))
         (= (p10 '()) '()))
  (println "p10 tests pass"))
