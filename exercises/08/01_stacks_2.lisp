;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 01_stacks_2.lisp - Zásobníkové programování
;;;
;;; Jazyk pro myšlený zásobníkový stroj.
;;; fáze 2 - vazby
;;;
;;; Předchozí soubor musí být načten.

;;
;; Slovník vazeb
;;

(defvar *bnd*)
(setf *bnd* `((pi . ,pi)))

;; Vytvoření vazby symbolu na hodnotu a její zrušení:
(define-word :bind
             (lambda ()
               (dict-push (pop *exec*) (pop *rslt*) *bnd*)))

(define-word :unbind
             (lambda () (pop *bnd*)))

;; Změna hodnoty vazby:
(define-word :set! (lambda ()
                     (dict-set (pop *exec*) (pop *rslt*)  *bnd*)))

;;
;; Vykonání symbolu
;;

(defun exec-symbol (sym)
  (push (dict-find sym *bnd*) 
        *rslt*))

;;
;; Spuštění programu
;;

;; Nové verze funkcí exec-step a execute
;; Nastavujeme proměnnou dspec:*redefinition-action*, abychom
;; potlačili warning o předefinování funkce z jiného souboru.
;; Nemusíte to řešit, ale můžete zkusit dát nastavení pryč, vyhodnotit
;; první soubor a pak zase tento soubor.
;; (proměnná je definována v LispWorks, není to standardní Common Lisp!)

(setf dspec:*redefinition-action* :quiet)

(defmethod exec-elem ((elem symbol))
  (if (wordp elem)
      (exec-word elem)
    (exec-symbol elem)))


(setf dspec:*redefinition-action* :warn)

#|

(execute 3 :bind 'x 4 :bind 'y 'x 'y :+ :unbind :unbind)

(execute 1 :bind 'a  'a :print 2 :set! 'a 'a :unbind)

;; obsah kruhu daného poloměru:
(define-word :carea
             '(:dup :* pi :*))

|#





