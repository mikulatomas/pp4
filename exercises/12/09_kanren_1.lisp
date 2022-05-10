;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 09_kanren_1.lisp - Logické programování
;;;
;;; Knihovna přidávající do lispu prvky logického programování.
;;; fáze 1 - základy
;;;
;;; Kanren (関連) znamená japonsky vztah. 
;;; web: http://minikanren.org

;; 
;; Term
;;

;; Data jsou reprezentovány termy.
;;
;; Vše co není pár ani proměnná (viz dále) je konstanta.
;; Symboly, čísla, řetězce jsou běžné konstanty.
;;
;; Například: 1, nil, "slunce", zero, ...
;;
;; Konstanty jsou termy.
;;
;; Pár, kde car i cdr je term, je opět termem.
;;
;; Například: (1 . 2), ("mezi" . (2 . "stromy")), ...
;;
;; Protože páry i nil jsou termy, libovolný seznam termů je opět termem.
;;
;; Například: (1 2 3), ((a b) (b c) (c d)), ...

;;
;; Proměnná
;;

;; Proměnné označují místa v termu, kam je možné dosadit jiný term.
;;
;; Proměnné mají tvar: ?číslo
;; kde číslo je celé nezáporné číslo. 
;;
;; Například: ?0, ?22, ...
;; 
;; Čísla proměnných volíme postupně od nuly.
;;
;; Proměnné jsou termy.
;;
;; Například v termu (1 . ?0) můžeme dosadit za proměnnou ?0 term 2
;; a získat term (1 . 2).

;; Proměnné budou instance třídy var.
(defclass var ()
  ((name :initarg :name)))

;; Vytvoření proměnné
(defun make-var (name)
  (make-instance 'var :name name))

;; Rovnost proměnných
(defun var-eq (var1 var2)
  (eql (slot-value var1 'name)
       (slot-value var2 'name)))

;; Upravíme printer a reader tak,
;; aby tiskl a zapisoval proměnné ve tvaru ?číslo. 

;; Tisk proměnných:
(defmethod print-object ((var var) stream)
  (princ "?" stream)
  (princ (slot-value var 'name) stream))

;; Funkce, kterou volá reader, pro přeštění proměnné.
(defun var-reader (stream char)
  (declare (ignore char))
  (make-var (read stream)))

;; Úprava readeru:
(set-macro-character #\? #'var-reader)


;; Dosazení termů za proměnné je zachyceno substitucí.
;;
;; Substituce je seznam tvaru:
;;   (sub (var1 . term1) ... (varn . termn))
;;
;; var1, ..., varn jsou proměnné
;; term1, ..., termn jsou temy
;;
;; Substituce vyjadřuje dosazení term1 za var1, ..., termn za varn.
;;
;; Například (sub (?0 . 1) (?1 . zero))
;; zachycuje dosazení 1 za ?0 a zero za ?1.

(defun sub (&rest pairs)
  `(sub ,@pairs))


;; Hledání výsledku dosazení proměnné:
(defun lookup (var sub)
  (find var (cdr sub) :key #'car :test #'var-eq))

#|
(lookup ?0 (sub '(?0 . a) '(?1 . b)))
(lookup ?0 (sub '(?0 . ?1) '(?1 . b)))
|#

;; Za proměnnou můžeme opět dosadit proměnnou.
;;
;; Například (sub (?0 . ?1) (?1 . b)) určuje dosazení ?1 za ?0.
;; 
;; Následující funkce se snaží dosadit za proměnnou term, 
;; který již proměnnou není.


;; Pokud je term proměnná:
(defmethod walk ((term var) sub)
  (let ((cons (lookup term sub)))
    (if cons
        (walk (cdr cons) sub)
      term)))

;; Jinak:
(defmethod walk (term sub)
  term)

#|
(walk ?0 (sub '(?0 . ?1) '(?1 . b)))
|#

;; Rozšíření subsituce o nové dosazení:
(defun extend-substitution (var term sub)
  `(sub (,var . ,term) ,@(cdr sub)))

#|
(extend-substitution ?1 'a (sub '(?0 . b)))
|#

;; Unifikace termů je problém nalezení takové substituce,
;; vzhledem ke které by se termy rovnaly.
;; 
;; Například termy (?0 b) a (a ?1) lze unifikovat substitucí (sub (?0 . a) (?1 . b)),
;; ale termy (?0 b) a (?1 a) unifikovat nelze.
;;
;; Obecněji vycházíme z jisté substituce 
;; a snažíme se ji rozšířit tak,
;; aby se termy rovnaly.
;;
;; Pomocná generická funkce unify-values rozlišuje unifikaci
;; pro různé typy termů.

;; Pokud jsou si termy rovny, pak vrať výchozí substituci.
;; (Použije se pro konstanty.)   
(defmethod unify-values (term1 term2 sub)
  (when (equalp term1 term2) 
    sub))

;; Pokud je jeden term proměnná, pak rozšiř substituci:
(defmethod unify-values ((term1 var) term2 sub)    
  (extend-substitution term1 term2 sub))

(defmethod unify-values (term1 (term2 var) sub)  
  (extend-substitution term2 term1 sub))  

;; Pokud jsou oba termy proměnné a jsou si rovny, pak vrať výchozí substituci: 
(defmethod unify-values ((term1 var) (term2 var) sub)
  (if (var-eq term1 term2)
      sub
    (call-next-method)))

;; U páru se snažíme unifikovat nejprve car a poté cdr.
(defmethod unify-values ((term1 cons) (term2 cons) sub)    
  (let ((sub (unify (car term1) (car term2) sub)))
    (when sub 
      (unify (cdr term1) (cdr term2) sub))))

;; Samotná funkce realizující unifikaci
;; nejprve případně dosadí za proměnné.  
(defun unify (term1 term2 sub)
  (let ((term1 (walk term1 sub))
        (term2 (walk term2 sub)))
    (unify-values term1 term2 sub)))

#|
(unify '(?0 b) '(a ?1) (sub))
(unify '(?0 b) '(a ?1) (sub '(?0 . c)))
|#

;; Stav výpočtu bude kromě substituce uchovávat
;; i počet dosud použitých proměnných. 
;; (Použije se pro vytváření takzavných čerstvých proměnných.)
;;
;; Stav je seznam (state sub var-count)
;; kde:
;;  - sub je substituce
;;  - var-count je počet dosud použitých proměnných

(defun state (sub var-count)
  `(state ,sub ,var-count))

(defun state-sub (state)
  (second state))

(defun state-var-count (state)
  (third state))
 
;; Výchozí stav má prázdnou substituci a nepoužili jsme žádnou proměnnou:
(defvar *empty-state* (state (sub) 0))

;; Stavy budeme organizovat do stromu. 
;; Nejprve si připomeneme definici stromů z prvního semestru.
;;
;; Uzel stromu je seznam: (tree hodnota . seznam-následníků)

(defun tree-node (val children)
  (cons 'tree (cons val children)))

(defun node-value (node)
  (cadr node))

(defun node-children (node)
  (cddr node))

;; Strom stavů je strom, kde hodnotou uzlu může být nil nebo stav.
;; Uzel může mít potomky, pouze pokud má hodnotu nil.
;; 
;; Například: (tree nil (tree (state (sub (?0 . a)) 1)) (tree (state (sub (?0 . b)) 1)))

;; Cíl je funkce, která očekává výchozí stav a vrací strom stavů, který cíl splňují.
;;
;; Například: (lambda (state) (make-tree state nil)) nebo (lambda (state) (make-tree nil nil))

;; Vrátí strom stavů cíle.
(defun apply-goal (goal state)
  (funcall goal state))

#|
(apply-goal (lambda (state) (tree-node state nil)) *empty-state*)
|#

;; Funkce == vrací pro dva termy cíl, který lze splnit tak,
;; že zadané termy unifikujeme vzhledem k výchozímu stavu.
(defun == (value1 value2)
  (lambda (state)
    (let ((sub (unify value1 value2 (state-sub state))))
      (if sub
          (tree-node (state sub (state-var-count state)) nil)
        (tree-node nil nil)))))



#|
(apply-goal (== ?0 'a) (state (sub) 1))
(apply-goal (== ?0 'b) (state (sub '(?0 . a)) 1))
|#

;; Ukážeme si techniku používání čerstvých proměnných.
;;
;; Proměnnou nazýváme čerstvou, pokud dosud při dosahování cíle nebyla použita.
;; (V jistém smyslu se jedná o alternativu lispového gensym.)
;;
;; Funkce call-fresh očekává funkci function a vrací cíl.
;; Funkce function musí pro proměnnou vracet cíl.
;;
;; Splnění cíle vráceného funkcí call-fresh probíhá v těchto krocích:
;; 1. Zavolá se funkce function na čerstvou proměnnou.
;; 2. Splníme cíl vrácený funkcí function.
  
(defun call-fresh (function)
  (lambda (state)
    (let ((var-count (state-var-count state)))
      (apply-goal (funcall function (make-var var-count)) 
                  (state (state-sub state) (+ var-count 1))))))


#|
(apply-goal (call-fresh (lambda (x)
                          (== x 'a)))
            *empty-state*)
|#

;; Funkce disj2 očekává dva cíle a vrací cíl, který je možné splnit tak,
;; že se splní cíl goal1 nebo goal2.
(defun disj2 (goal1 goal2)
  (lambda (state)
    (tree-node nil (list (apply-goal goal1 state)
                         (apply-goal goal2 state)))))


#|
(apply-goal (disj2 (== ?0 'a) (== ?0 'b)) (state (sub) 1))
|#


;; Splnění cíle pro každý stav stromu.
(defun state-tree-apply (goal tree)
  (if (node-value tree)
      (apply-goal goal (node-value tree))
    (tree-node nil (mapcar (lambda (child)
                             (state-tree-apply goal child))
                           (node-children tree)))))

#|
(state-tree-apply (== ?0 1)
                  (tree-node nil (list (tree-node (state (sub '(?0 . 1)) 1) nil)
                                       (tree-node (state (sub '(?0 . 2)) 1) nil))))
|#
    

;; Funkce conj2 očekává dva cíle a vrací cíl, 
;; který lze splnit tak, že se splní oba zadané cíle.

(defun conj2 (goal1 goal2)
  (lambda (state)
    (let ((states1 (apply-goal goal1 state)))
      (state-tree-apply goal2 states1))))

#|
(apply-goal (conj2 (== ?0 'a) (== ?1 'b)) (state (sub) 2))
|#




#|
;; Vše dohromady:
(apply-goal (call-fresh 
             (lambda (a)
               (call-fresh 
                (lambda (b)
                  (disj2 (conj2 (== a 1)
                                (== b 2))
                         (conj2 (== a 3)
                                (== b 4)))))))
            *empty-state*)
|#

;;
;; Procházení stromu stavů
;;

;; Upravíme procházení stromů do hloubky a do šířky z prvního semestru tak, 
;; aby lépe vyhovovalo zpracování stromu stavů. 

;; Nepovinný parametr fun určuje funkci, která se bude aplikovat na každý stav ve stromu.
(defun state-tree-values-dfs (root &optional (fun #'identity))
  (if (node-value root)
      (list (funcall fun (node-value root)))
    (mapcan (lambda (child) (state-tree-values-dfs child fun)) 
            (node-children root))))

#|
(state-tree-values-dfs (tree-node nil (list (tree-node nil (list (tree-node nil nil) 
                                                                 (tree-node (state (sub '(?0 . 3)) 1) nil)))
                                            (tree-node (state (sub '(?0 . 1)) 1) nil) 
                                            (tree-node (state (sub '(?0 . 2)) 1) nil))))
|#

(defun state-tree-values-bfs (root &optional (fun #'identity))
  (state-tree-values-bfs-multi (list root) fun))

(defun state-tree-values-bfs-multi (roots fun)
  (when roots
    (append (mapcar fun
                    (remove nil
                            (mapcar #'node-value roots)))
            (state-tree-values-bfs-multi (mapcan #'node-children roots) fun))))

#|
(state-tree-values-bfs (tree-node nil (list (tree-node nil (list (tree-node nil nil) 
                                                                 (tree-node (state (sub '(?0 . 3)) 1) nil)))
                                            (tree-node (state (sub '(?0 . 1)) 1) nil) 
                                            (tree-node (state (sub '(?0 . 2)) 1) nil))))
|#

#|
(setf tree (apply-goal (call-fresh 
                        (lambda (a)
                          (disj2 (disj2 (== a 1)
                                        (== a 2))
                                 (== a 3))))
                       *empty-state*))

(state-tree-values-dfs tree #'state-sub)
(state-tree-values-bfs tree #'state-sub)
|#
  