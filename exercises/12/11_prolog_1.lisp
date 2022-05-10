;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 11_prolog_1.lisp - Logické programování
;;;
;;; Prolog.
;;;

;; Nově povolíme symbol, jako název proměnné.
;; Například: ?x, ?var, ?first-value, ...

;; Termy v Prologu mají jiný význam než v Kanren.
;; Termům v Kanren budeme dále říkat hodnoty.

;; Proměnná v Prologu musí mít za název symbol.

;; Uvažujeme tři typy symbolů:
;; 1) konstanty
;; 2) funkční symboly
;; 3) relační symboly

;; U funkčních a relačních symbolů uvažujeme aritu.

;; Termy:
;; 1) Každá konstanta je termem.
;; 2) Každá proměnná je termem.
;; 3) Pokud f je funkční symbol arity n a t1, ..., tn jsou termy, pak (f t1 ... tn) je term.

;; Atomické formule:
;; Pokud r je relační symbol arity n a t1, ..., tn jsou termy, pak (r t1 ... tn) je atomická formule.

;; Termy i atomické formule jsou z pohledu Kanren hodnoty.

;; Pravidlo je seznan (<- head . body) kde
;; - head (hlava) je atomická formule
;; - body (tělo) je seznam atomických formulí

(defun head (rule)
  (cadr rule))

(defun body (rule)
  (cddr rule))

;; Program je seznam pravidel.
(defvar *program*)
(setf *program* nil)

;; Přidání pravidla do programu
;; (Přidáváme nakonec, aby bylo zachované pořadí pravidel.)

(defmacro <- (head &rest body)
  `(setf *program* (append *program* '((<- ,head ,@body)))))

;; Čísla
;; - symbol number je relační symbol arity jedna
;; - symbol succ je funkční symbol arity jedna
;; - symbol zero je konstanta
(<- (number zero))
(<- (number (succ ?x)) (number ?x))

;; Přejmenování proměnných v hodnotě na čerstvé.
;; Vrací seznam: (value state)

(defmethod fresh-val ((val var) state)
  (let ((pair (lookup val  (state-sub state))))
    (if pair
        (list (cdr pair) state)
      (let ((var (make-var (state-var-count state))))
        (list var (state (extend-substitution val var (state-sub state)) (1+ (state-var-count state))))))))
#|
(fresh-val ?a (state (sub '(?a . ?0)) 1))
(fresh-val ?a (state (sub) 0))
|#

(defmethod fresh-val ((val cons) state)
  (let* ((car-res (fresh-val (car val) state))
         (cdr-res (fresh-val (cdr val) (second car-res))))
      (list (cons (first car-res)
                  (first cdr-res))
            (second cdr-res))))

#|
(fresh-val '(?a . ?b) *empty-state*)
|#

(defmethod fresh-val (val state)
  (list val state))

;; Makro with-fresh-val

;; syntax: (with-fresh-val (var val &optional sub) . goals)
;;
;; Vrátí cíl, který vyhodnotí val, přejmenuje proměnné hodnoty val na čerstvé
;; a vytvoří vazbu proměnné var na vzniklou hodnotu.
;; Dále se pokusí splnit cíle goals.
;; Pokud je zadáno sub (musí to být symbol), pak je v definici cílů
;; na symbol sub navázána substituce určující nahrazení čerstvých proměnných.

(defmacro with-fresh-val ((var val &optional (sub (gensym "SUB"))) &body goals)
  (let ((res (gensym "RES"))
        (state (gensym "STATE")))
    `(lambda (,state)
       (let ((,res (fresh-val ,val (state (sub) (state-var-count ,state)))))
         (let ((,var (first ,res))
               (,sub (state-sub (second ,res))))
           (apply-goal
            (conj ,@goals)
            (state (state-sub ,state) (state-var-count (second ,res)))))))))

#|
(run t (x) 
  (with-fresh-val (v `(?a ?b))
    (== v x)))

(run t (x) 
  (with-fresh-val (v `(?a ?b) sub)
    (== x sub)))
|#
;; Cíl (goal) je seznam (-> atom1 atom2 ...) 
;; kde atom1 atom2 ... jsou atomické formule nazývané podcíle (subgoals).

(defun subgoals (goal)
  (cdr goal))

(defun apply-rule (rule goal)
  `(-> ,@(body rule) ,@(cdr (subgoals goal))))

#|
(apply-rule '(<- (number (succ zero)) (number zero)) '(-> (number (succ zero)) (number ?x)))
|#

;; Resoluce
(defun resolution-step (goal rule)
  (conj
   (== (car (subgoals goal)) (head rule))
   (resolution (apply-rule rule goal))))
                
(defun resolution (goal)
  (disj
   (== goal '(->))
   (with-fresh-val (program *program*)
     (apply #'disj (mapcar (lambda (rule)
                             (resolution-step goal rule))
                           program)))))

;; Hlavní cíl prologu
(defun prologo (var goal)
  (with-fresh-val (fresh-goal goal sub)
    (conj
     (== var sub)
     (resolution fresh-goal))))
     
#|
(run 3 (x)
  (prologo x '(-> (number ?x))))
|#

;; Přehlednější tisk substituce (vazby se tisknou v opačném pořadí).
(defun print-sub (sub)
  (labels ((print-pairs (pairs)
             (when pairs
               (let ((pair (car pairs)))
                 (princ (car pair))
                 (princ " = ")
                 (princ (cdr pair))
                 (when (cdr pairs)
                   (princ ", ")))
               (print-pairs (cdr pairs)))))
    (print-pairs (reverse (cdr sub)))))

#|
(print-sub (sub '(?a . zero) '(?b . (succ zero))))
|#

;; Interaktivní tisk proudu výsledků
(defun print-results (result-stream)
  (let ((sub (stream-car result-stream)))
    (if (cdr sub)
        (print-sub sub)
      (princ "success"))
    (let ((rest (stream-cdr result-stream)))
      (cond 
       (rest
        (format t "~%Next? (y - yes, n - no): ")
        (let ((char (read-char)))
          (when (eql char #\y)
             (format t "~%~%")
            (print-results rest))))
       (t
         (format t "~%end "))))))

#|
(print-results (run nil (x) (prologo x '(-> (number ?x)))))
|#

;; Interaktivní splnění cílů a tisk výsledků
(defun prolog-eval-print (goal)
  (let ((results (run nil (a)
                   (prologo a goal))))
      (if results
          (print-results results)
        (format t "fail"))))

#|
(prolog-eval-print '(-> (number ?x)))
|#

;; Pro pohodlné zadávání cílů.      
(defmacro -> (&body goal)
  `(prolog-eval-print '(-> ,@goal)))

#|
(-> (number ?x))
(-> (number ?x) (number ?y))
|#


;; Plnění cílů průchodem do hloubky:
(defmacro d-> (&body goal)
  `(let ((*dfsp* t))
     (-> ,@goal)))
      

#|
(d-> (number ?x))
(d-> (number ?x) (number ?y))
|#


(<- (fives1 ?x) (fives1 ?x))
(<- (fives1 five))

#|
(-> (fives1 ?x))

;; Nekonečný výpočet:
(d-> (fives1 ?x))
|#

(<- (fives2 five))
(<- (fives2 ?x) (fives2 ?x))

#|
(-> (fives2 ?x))
(d-> (fives2 ?x))
|#


(<- (fours2 four))
(<- (fours2 ?x) (fours2 ?x))

(<- (fives-or-fours ?x) (fives2 ?x))
(<- (fives-or-fours ?x) (fours2 ?x)) 

#|
(-> (fives-or-fours ?x))
(d-> (fives-or-fours ?x))
|#

(<- (even zero))
(<- (even (succ (succ ?n))) (even ?n))

#|
(-> (even (succ (succ zero))))
|#

;; Součet
(<- (add zero ?x ?x) (number ?x))
(<- (add (succ ?x) ?y (succ ?z)) (add ?x ?y ?z))

#|
(-> (add (succ zero) (succ zero) ?x))
(-> (add (succ zero) ?x (succ (succ zero))))
(-> (add ?x ?y (succ (succ zero))))
(-> (add ?x ?y ?z))
(d-> (add ?x ?y ?z))
|#

