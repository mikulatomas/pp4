;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 12_prolog_3.lisp - Logické programování
;;;
;;; Prolog.
;;; Zásobníkový interpret Prologu.
;;;
;;; Předchozí soubory musejí být načteny.

#|
Současný interpret Prologu je velmi neefektivní.
Vytvoříme interpret založený na zásobníkovém výpočtu.
Nepoužijeme stávající zásobníkový stroj, ale pro jednoduchost
si vytvoříme nový vysoce specializovaný právě 
pro interpretaci Prologu.

Výzvou by bylo přepsat interpret tak, aby používal
již existující zásobníkový stroj.
Lze využít jeho rozšíření pro praralelní výpočty?
|#

;; Na zásobník budeme ukládat stavy výpočtu Prologu.
;; Stav výpočtu Prologu je seznam (pstate cíl substituce počet-proměnných)

(defun pstate (goal sub var-count)
  (list 'pstate goal sub var-count))

(defun pstate-goal (pstate)
  (second pstate))

(defun pstate-sub (pstate)
  (third pstate))

(defun pstate-var-count (pstate)
  (fourth pstate))

;; Stavy ukládáme na zásobníky (prolog states)
(defvar *psts*) 

;; Pokud je proměnná *trace* Pravda, interpret tiskne průběh výpočtu.
(defvar *trace* nil)

;; Vytvoří a přidá stav na zásobník.
(defun add-pstate (goal sub var-count)
  (push (pstate goal sub var-count) *psts*))

;; Vytiskne informace o kroku výpočtu
(defun print-trace (goal rule sub new-goal new-goal-sub)
   (format t "~%~%Level: ~a " (length *psts*))
   (format t "~%Goal: ")
   (pprint (decode goal))
   (format t "~%Rule:")
   (pprint (decode rule))
   (format t "~%Substitution: ")
   (print-sub sub)
   (format t "~%New goal: ")
   (pprint (decode new-goal))
   (format t "~%Goal substitution: ")
   (print-sub new-goal-sub))

;; Jeden krok rezoluce.
;; Už víme, že pravidlo lze použít.
(defun native-resolution-step (goal rule goal-sub var-count sub)
  (let ((new-goal (recursive-walk (apply-rule rule goal) sub))
        (new-goal-sub (recursive-walk goal-sub sub)))
    (when *trace*
      (print-trace goal rule sub new-goal new-goal-sub))
    (add-pstate new-goal
                new-goal-sub
                var-count)))

;; Pokusíme se aplikovat pravidlo na cíl.
(defun try-apply-rule (goal rule goal-sub var-count)
  (let ((sub (unify (car (subgoals goal)) (head rule) (sub))))
    (when sub
      (native-resolution-step goal rule goal-sub var-count sub))))

;; Postupně se snažíme aplikovat všechny pravidla v programu.  
(defun execute-non-empty-goal (pstate)
  (let ((goal (pstate-goal pstate))
        (sub (pstate-sub pstate))
        (var-count (pstate-var-count pstate)))
    (dolist (rule (reverse *program*))
      (let ((res (fresh-val rule (state (sub) var-count))))
        (try-apply-rule goal (first res) sub (state-var-count (second res)))))))

;; Tisk výsledků.
(defun native-print-result (pstate)
  (when *trace*
    (format t "~%~%Result: "))
  (print-result (pstate-sub pstate)))
  
;; Krok výpočtu
(defun resolution-execute-step ()
  (let ((pstate (pop *psts*)))
    (cond
     ((subgoals (pstate-goal pstate))
      (execute-non-empty-goal pstate))
     (t
      (native-print-result pstate)
      (unless (ask-for-next)
        (setf *psts* nil))))))

;; Iterativní výpočet
(defun resolution-execute (pstate)
  (let ((*psts* (list pstate)))
    (loop 
     (unless *psts* (return))
     (resolution-execute-step))))

(defun prolog (goal)
  (let ((res (fresh-val (encode goal) (state (sub) 0))))
    (let ((fresh-goal (first res))
          (goal-sub (state-sub (second res)))
          (var-count (state-var-count (second res))))
      (when *trace*
        (format t "~%Goal substitution: ")
        (print-sub goal-sub))
      (resolution-execute (pstate fresh-goal goal-sub var-count)))))

(defmacro n-> (&body goal)
  `(prolog '(-> ,@goal)))


#|

;; Test:
(let ((*program* '()))
  (<- (even 0))
  (<- (even (succ (succ ?x))) 
      (even ?x))
  (n-> (even ?x)))


;; a s trasováním
(let ((*program* '())
      (*trace* t))
  (<- (even 0))
  (<- (even (succ (succ ?x))) 
      (even ?x))
  (n-> (even ?x)))
|#

