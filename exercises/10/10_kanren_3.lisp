;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 10_kanren_3.lisp - Logické programování
;;;
;;; Knihovna přidávající do lispu prvky logického programování.
;;; fáze 3 - prohledávání nekonečných líných stromů stavů
;;;
;;; Předchozí soubory musejí být načteny.

;;
;; Přísliby
;;

(defun make-promise (fun)
  (list 'promise nil nil fun))

(defmacro delay (expr)
  `(make-promise (lambda () ,expr)))

(defun validp (promise)
  (second promise))

(defun force (promise)
  (unless (validp promise)
    (setf (third promise) (funcall (fourth promise))
          (second promise) t))
  (third promise))
 
(defun invalidate (promise)
  (setf (second promise) nil))

;;
;; Proudy
;;

(defmacro cons-stream (car cdr)
  `(cons ,car (delay ,cdr)))

(defun stream-car (stream)
  (car stream))

(defun stream-cdr (stream)
  (force (cdr stream)))

(defun list-to-stream (list)
  (if (null list)
      '()
    (cons-stream (car list) (list-to-stream (cdr list)))))

(defun stream-to-list (stream &optional max-count)
  (if (or (null stream)
          (eql max-count 0))
      '()
    (cons (stream-car stream) 
          (stream-to-list (stream-cdr stream)
                          (when max-count (- max-count 1))))))

;; Druhý argument je příslib proudu.
(defun stream-append-fun (stream1 stream2-promise)
  (if (null stream1)
      (force stream2-promise)
    (cons-stream (stream-car stream1) 
                 (stream-append-fun (stream-cdr stream1) stream2-promise))))

(defmacro stream-append (stream1 stream2)
  `(stream-append-fun ,stream1 (delay ,stream2)))
#|
(stream-to-list (stream-append (list-to-stream '(1 2 3))
                               (naturals)) 
                5)
|#


(defun stream-mapcan (f s)
  (if (null s)
      '()
    (stream-append
     (funcall f (stream-car s))
     (stream-mapcan f (stream-cdr s)))))

#|
(stream-to-list (stream-mapcan (lambda (v) (list-to-stream (list v v))) 
                               (naturals)) 
                5)
|#

;;
;; Líné stromy.
;;

;; Uzel líného stromu je seznam: (lazy-tree hodnota . příslib-seznamu-následníků)
(defmacro lazy-tree-node (val children)
  `(cons 'lazy-tree (cons ,val (delay ,children))))

(defun lazy-node-value (node)
  (cadr node))

(defun lazy-node-children (node)
  (force (cddr node)))

;; Procházení líného stromu stavů
(defun lazy-state-tree-dfs (root &optional (fun #'identity))
  (if (lazy-node-value root)
      (cons-stream (funcall fun (lazy-node-value root)) nil)
    (stream-mapcan (lambda (child) (lazy-state-tree-dfs child fun)) 
                   (list-to-stream (lazy-node-children root)))))


#|
(stream-to-list (lazy-state-tree-dfs 
                 (lazy-tree-node nil 
                                 (list (lazy-tree-node nil 
                                                       (list (lazy-tree-node (state (sub '(?0 . a)) 1) nil)))
                                       (lazy-tree-node (state (sub '(?0 . b)) 1) nil)))))
|#

(defun lazy-state-tree-bfs (root &optional (fun #'identity))
  (lazy-state-tree-bfs-multi (list root) fun))

(defun lazy-state-tree-bfs-multi (roots fun)
  (when roots
    (stream-append (list-to-stream (mapcar fun
                                           (remove nil
                                                   (mapcar #'lazy-node-value roots))))
            (lazy-state-tree-bfs-multi (mapcan #'lazy-node-children roots) fun))))


#|
(stream-to-list (lazy-state-tree-bfs 
                 (lazy-tree-node nil 
                                 (list (lazy-tree-node nil 
                                                       (list (lazy-tree-node (state (sub '(?0 . a)) 1) nil)))
                                       (lazy-tree-node (state (sub '(?0 . b)) 1) nil)))))
|#

;; ==, disj2 a conj2 musí vracet líný strom

(setf dspec:*redefinition-action* :quiet)

(defun == (value1 value2)
  (lambda (state)
    (let ((sub (unify value1 value2 (second state))))
      (if sub
          (lazy-tree-node (state sub (state-var-count state)) nil)
        (lazy-tree-node nil nil)))))

(setf dspec:*redefinition-action* :warn)

#|
(stream-to-list (lazy-state-tree-bfs (apply-goal (== ?0 'a) (state (sub) 1))))
|#

(setf *success* (== t t))
(setf *fail* (== t nil))

(setf dspec:*redefinition-action* :quiet)

(defun disj2 (goal1 goal2)
  (lambda (state)
    (lazy-tree-node nil (list (apply-goal goal1 state)
                              (apply-goal goal2 state)))))

(setf dspec:*redefinition-action* :warn)

#|
(stream-to-list (lazy-state-tree-bfs (apply-goal (disj2 (== ?0 'a) (== ?0 'b)) (state (sub) 1))))
|#

(defun lazy-state-tree-apply (goal tree)
  (if (lazy-node-value tree)
      (apply-goal goal (lazy-node-value tree))
    (lazy-tree-node nil (mapcar (lambda (child)
                                  (lazy-state-tree-apply goal child))
                           (lazy-node-children tree)))))

(setf dspec:*redefinition-action* :quiet)

(defun conj2 (goal1 goal2)
  (lambda (state)
    (let ((states1 (apply-goal goal1 state)))
      (lazy-state-tree-apply goal2 states1))))

(setf dspec:*redefinition-action* :warn)

#|
(stream-to-list (lazy-state-tree-bfs (apply-goal (conj2 (== ?0 'a) (== ?1 'b)) (state (sub) 2))))
|#
    
(defun goal-results-stream (goal)
  (funcall (if *dfsp* #'lazy-state-tree-dfs #'lazy-state-tree-bfs)
           (apply-goal goal *empty-state*)
           (lambda (state)
             (recursive-walk ?0 (state-sub state)))))

(defun run-goal-lazy (n goal)
  (let ((stream (goal-results-stream goal)))
    (if n
        (stream-to-list stream (and (not (eql n t)) n))
      stream)))
  

(setf dspec:*redefinition-action* :quiet)

(defmacro run (n vars &body formulas)
  `(run-goal-lazy ,n (with-fresh ,vars 
                       ,@formulas)))

(setf dspec:*redefinition-action* :warn)


(defun fives1 (x)
  (disj (== x 5)
        (fives1 x)))

#|
;; Zacyklí se:
(run 5 (x) (fives1 x))
|#

;; Makro na uspání cíle.
(defmacro zzz (goal)
  (let ((state (gensym "STATE")))
    `(lambda (,state)
       (apply-goal ,goal ,state))))


(defun fives2 (x)
  (disj 
   (== x 5)
   (zzz (fives2 x))))

#|
;; V pořádku:
(run 5 (x) (fives2 x))

;; Lze prohledávat i do hloubky:
(let ((*dfsp* t))
  (run 5 (x) (fives2 x)))
|#

(defun fives3 (x)
  (disj
   (zzz (fives3 x))
   (== x 5)))


#|
;; Prohledávání do šířky je v pořádku:
(run 5 (x) (fives3 x))

;; Prohledávání do hloubky se zacyklí:
(let ((*dfsp* t))
  (run 5 (x) (fives3 x)))
|#

(defun fours2 (x)
  (disj
   (== x 4)
   (zzz (fours2 x))))

#|

;; Pětky i čtyřky:
(run 10 (x) (disj (fives2 x)
                  (fours2 x)))

;; Pouze pětky:
(let ((*dfsp* t))
  (run 10 (x) (disj (fives2 x)
                    (fours2 x))))
|#

;; Je x nula?
(defun zeroo (x)
  (== x 'zero))

;; Je y předchůdcem x?
(defun predo (x y)
  (== x `(succ ,y)))

(defun numbero (x)
  (disj
    (zeroo x)
    (with-fresh (y)
      (predo x y)
      (numbero y))))

#|
(run 3 (x) (numbero x))

(let ((*dfsp* t)) 
  (run 3 (x) (numbero x)))
|#

;; Jiná definice predikátu na čísla:
(defun numbero2 (x)
  (disj
   (with-fresh (y)
     (predo x y)
     (numbero2 y))
   (zeroo x)))

#|
(run 3 (x) (numbero2 x))

;; Průchod do hloubky nefunguje:
(let ((*dfsp* t)) 
  (run 3 (x) (numbero2 x)))
|#

      