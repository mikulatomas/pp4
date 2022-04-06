;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 09_kanren_2.lisp - Logické programování
;;;
;;; Knihovna přidávající do lispu prvky logického programování.
;;; fáze 1 - uživatelská vrstva
;;;

;; Vždy splněný cíl a nikdy nesplněný cíl:
(defvar *success* (== t t))
(defvar *fail* (== t nil))

;; Disjunkce obecného počtu cílů.
(defun disj (&rest goals)
  (if goals
      (disj2 (car goals) (apply #'disj (cdr goals)))
    *fail*))

#|
(apply-goal (disj (== ?1 'a) (== ?1 'b) (== ?1 'c)) *empty-state*)   
|#

;; Konujunkce libovolného počtu číslů.
(defun conj (&rest goals)
  (if goals
      (conj2 (car goals) (apply #'conj (cdr goals)))
    *success*))

#|
(apply-goal (conj (== ?1 'a) (== ?2 'b) (== ?3 'c)) *empty-state*) 
|#

;; Makro conde
;;
;; Syntax: 
;; (conde
;;   ((goal11 goal12 ...))
;;   ...
;;   ((goaln1 goaln2 ...)))
;;
;; Definuje cíl, který je splněn, pokud existuje číslo i takové,
;; že jsou splněny všechny cíle goali1 goali2 ...
;;
;; Jedná se o logickou obdobu makra cond.
(defmacro conde (&body clauses)
  (if clauses
      `(disj (conj ,@(car clauses))
             (conde ,@(cdr clauses)))
    '(disj)))

#|
(apply-goal (conde 
              ((== ?0 'a)
               (== ?1 'c))
              ((== ?0 'b)
               (== ?1 'd)))
            `(state (sub (?0 . a)) 0))
|#


;; Makro na vytváření čerstvých proměnných
;; Syntax:
;; (with-fresh (var1 ... varn) . goals)
;;
;; Definuje cíl, který je splněn, pokud jsou splněny všechny cíle goals.
;; Při určování cílů goals jsou k dispozici čerstvé proměnné
;; navázané na symboly var1, ..., varn. 
;;               
(defmacro with-fresh (vars &body body)
  (if vars
      `(call-fresh (lambda (,(car vars))
                     (with-fresh (,@(cdr vars))
                       ,@body)))
    `(conj ,@body)))

#|
(apply-goal (with-fresh (a b c)
              (== a 1)
              (== b 2)
              (== c 3))
            *empty-state*)
|#


;; Rekurzivní dosazování do termu. 
(defmethod structure-walk ((term cons) sub)
  (cons (recursive-walk (car term) sub)
        (recursive-walk (cdr term) sub)))

(defmethod structure-walk (term sub)
  term)

(defun recursive-walk (term sub)
  (let ((value (walk term sub)))
    (structure-walk value sub)))

#|
(recursive-walk '(?1 ?2) '(sub (?1 . a) (?2 . b)))
|#

;;
;; Funkce, kterou bude volat uživatel.
;;

;; Syntax: (run n (var1 ... varn) . formulas)
;; - n je číslo nebo t
;; - var1 ... varn jsou symboly
;; – formulas je seznam logických fomulí.
;;
;; Makro vyhodnotí formule v prostředí, 
;; kde na symboly var1 ... varn, budou navázány na čersvé proměnné.
;;
;; Tím obdrží cíle.
;;
;; Dále zjistí strom stavů, které splňují všechny cíle.
;;
;; Poté získá seznam stavů obsažených ve stromu.
;;
;; Pokud je *dfsp* Pravda, 
;; prochází se strom stavů do hloubky, jinak do šířky. 
;;
;; Získá seznam výsledků, 
;; kde výsledek je dosazení za proměnnou var1 
;; vzhledem k substituci obdrženého stavu.
;;
;; Pokud je n číslo, vrátí seznam prvních n výsledků.
;; Pokud je n hodnota t, vrátí celý seznam.

(defvar *dfsp* nil)

(defun goal-results (goal)
  (funcall (if *dfsp* #'state-tree-values-dfs #'state-tree-values-bfs)
           (apply-goal goal *empty-state*)
           (lambda (state)
             (recursive-walk ?0 (state-sub state)))))

(defun run-goal (n goal)
  (let ((results (goal-results goal)))
    (if n
        (subseq results 0 (if (eql n t) nil n))
      results)))


(defmacro run (n vars &body formulas)
  `(run-goal ,n (with-fresh ,vars 
                      ,@formulas)))

#|
(run t (a b c)
  (==  a `(,b ,c))
  (conde
    ((== b 1)
     (== c 2))
    ((== b 3)
     (== c 4))))
|#
;;
;; Příklad - seznamy
;;

;; Predikáty jsou lispové funkce. Na konec názvu funkce budeme dávat písmeno 'o',
;; aby nedošlo ke konfliktu s existujícími funkcemi.

;; Predikát nullo je splněn, pokud je x hodnota nil.
(defun nullo (x)
  (== nil x))

#|
(run t (x) (nullo nil))
(run t (x) (nullo '(1)))
(run t (x) (nullo x))
|#

;; Predikát conso je splněn, pokud pár pair má první složku car a druhou cdr.
(defun conso (car cdr pair)
  (== (cons car cdr) pair))

#|
(run t (x)
  (conso 1 2 x))

(run t (x y z)
  (== x (list y z))
  (conso y z '(1 . 2)))
|#

;; Predikát membero je splněn, pokud el je prvkem seznamu list.
(defun membero (el list)
  (with-fresh (first rest)
    (conso first rest list)
    (conde
      ((== el first))
      ((membero el rest)))))
     
#|
(run t (x)
  (membero x '(1 2 3)))

(run t (x)
  (membero 2 `(1 ,x 3)))

(run t (x)
  (membero x '(1 2 3))
  (membero x '(2 3 4)))

(run t (x)
  (disj (membero x '(1 2 3))
        (membero x '(2 3 4))))

;; Prohledáváním do hlouhky obdržíme jiné pořadí výsledků: 
(let ((*dfs* t))
  (run t (x)
    (disj (membero x '(1 2 3))
          (membero x '(2 3 4)))))
  

;; Nekonečný výpočet:
(run t (x)
  (membero 1 x))

;; Nepomůže: 
(run 1 (x)
  (membero 1 x))
|#


