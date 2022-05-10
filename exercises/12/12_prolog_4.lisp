;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 12_prolog_4.lisp - Logické programování
;;;
;;; Prolog.
;;; Sudá čísla a algoritmus rychlého třízení (quick sort).
;;;
;;; Předchozí soubory musejí být načteny.

#|
Definování nových pravidel neruší pravidla již existující,
proto doporučuji při každé změně vždy načíst celý soubor.

Na začátku totiž předchozí program Prologu smažeme.
|#

;; Smažeme existující program:
(setf *program* nil)

;; even
(<- (even 0))

(<- (even (succ (succ ?n))) 
    (even ?n))

#|
;; Používáme efektivnější implementaci Prologu:
(n-> (even (succ ?x)))

;; Výpočet s trasováním:
(let ((*trace* t))
  (n-> (even (succ ?x))))
|#

;; <=
(<- (<= 0 ?x))

(<- (<= (succ ?x) (succ ?y)) 
    (<= ?x ?y))

#|
(n-> (<= ?x 5))
|#

;; <
(<- (< 0 (succ ?x)))

(<- (< (succ ?x) (succ ?y)) 
    (< ?x ?y))

#|
(n-> (< ?x 5))
|#

;; append
(<- (append '() ?xs ?xs))

(<- (append (cons ?x ?xs) ?ys (cons ?x ?zs)) 
    (append ?xs ?ys ?zs))

#|
(n-> (append '(1 2) '(3 4) ?x))
(n-> (append '(1 2) ?x '(1 2 3 4)))
(n-> (append ?x ?y '(1 2 3 4)))
|#

;; Quick Sort
(<- (qsort '() '()))

(<- (qsort (cons ?h ?t) ?s) 
    (qsplit ?h ?t ?a ?b) 
    (qsort ?a ?a1)
    (qsort ?b ?b1)
    (append ?a1 (cons ?h ?b1) ?s))

#|
(n-> (qsort '(1 5 4 2) ?x))
|#

;; split pro Quick Sort
(<- (qsplit ?h (cons ?a ?x) (cons ?a ?y) ?z)
    (<= ?a ?h)
    (qsplit ?h ?x ?y ?z))

(<- (qsplit ?h (cons ?a ?x) ?y (cons ?a ?z))
    (< ?h ?a)
    (qsplit ?h ?x ?y ?z))

(<- (qsplit ?x '() '() '()))

#|
(n-> (qsplit 2 '(1 5 4 2 3) ?x ?y))
|#