;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 12_prolog_2.lisp - Logické programování
;;;
;;; Prolog.
;;; Kódování čísel a seznamů do termů.
;;;
;;; Předchozí soubory musejí být načteny.

#|
Přirozená čísla s nulou kódujeme pomocí succ a zero.
Například 2 zakódujeme jako (succ (succ zero)).

Poznámka: Nejedná se zrovna o efektivní způsob kódování čísel. 
Takto můžeme prakticky zakódovat jen poměrně malá čísla.
Lepší by bylo přejít například na kódování čísel binárně. 

Seznamy kódujeme pomocí cons a empty.
Například seznam '(a b) zakódujeme jako (cons a (cons b empty)).

Seznamy je vždy potřeba kvótovat a to i prázdný seznam '().
|#
          
;; Proměná rozhoduje, zda se nacházíme v zakvotovaném výrazu.
(defvar *quoted* nil)
  
;; Kódování hodnot do termů        

(defmethod encode (val)
  val)

;; Kódování čísel 

(defmethod encode ((val (eql 0)))
  'zero)

(defmethod encode ((val integer))
  `(succ ,(encode (- val 1))))

#|
(encode 0)
(encode 5)
|#

;; Kódování seznamů
(defmethod encode ((val (eql '())))
  (if *quoted*
      'empty
    val))


(defmethod encode ((val cons))
  (cond
   (*quoted*
    `(cons ,(encode (car val))
           ,(encode (cdr val))))
   ((eql (car val) 'quote)
    (let ((*quoted* t))
      (encode (cadr val))))
   (t
    (cons (encode (car val))
          (encode (cdr val))))))

#|
(encode ''(a b c))
(encode ''((a) (b c)))
|#


#|
(encode '(append '(1 2) '(3 4) ?x))
|#

;; Odkódování hodnot

(defmethod decode (val)
  val)

;; Způsob odkódování seznamu určí první prvek
(defmethod decode ((val cons))
  (decode-op (car val) (cdr val)))

(defmethod decode-op (op args)
  `(,op ,@(mapcar #'decode args)))


;; Odkódování čísel
(defmethod decode ((val (eql 'zero)))
  0)

(defmethod decode-op ((fun (eql 'succ)) args)
  (let ((val (decode (car args))))
    (if (integerp val)
        (1+ val)
      `(succ ,val))))

#|
(decode '(succ (succ zero)))
(decode '(succ (succ ?x)))
|#

;; Odkódovíní seznamů
(defmethod decode ((val (eql 'empty)))
  (if *quoted*
      nil
    ''nil))

 
(defmethod decode-op ((fun (eql 'cons)) args)
  (let ((cons
         (let ((*quoted* t))
           (cons
            (decode (car args))
            (decode (cadr args))))))
    (if *quoted*
        cons
      `',cons)))

#|
(decode '(cons (cons a empty) (cons b empty)))

;; Lepší tisk:
(pprint (decode '(cons (cons a empty) (cons b empty))))
|#


#|
(decode '(cons (succ zero) (cons (succ (succ zero)) empty)))
(decode '(append (cons (succ zero) (cons (succ (succ zero)) empty)) empty ?x))
|#

#|
Upravíme stávající interpret prologu tak, aby používal kódování hodnot. 
Současně zlepšíme čitelnost některých dříve napsaných funkcí.
|#

(setf dspec:*redefinition-action* :quiet)

;; Zakódujeme hodnoty v cíli
(defun prolog-eval-print (goal)
  (print-results
   (run nil (a)
     (prologo a (encode goal)))))

(setf dspec:*redefinition-action* :warn)


;; Tiskne jednu vazbu v substituci.
;; Hodnotu odkóduje.
(defun print-sub-pair (pair)
  (princ (car pair))
  (princ " = ")
  (let ((*print-pretty* t))
    (princ (decode (cdr pair)))))

;; Tiskne vazby v substituci.
(defun print-sub-pairs (pairs)
  (when pairs
    (print-sub-pair (car pairs))
    (when (cdr pairs)
      (princ ", "))
    (print-sub-pairs (cdr pairs))))
  
(setf dspec:*redefinition-action* :quiet)

;; Tiskne substituci.
(defun print-sub (sub)
  (format t "~%")
  (print-sub-pairs (reverse (cdr sub))))

(setf dspec:*redefinition-action* :warn)

;; Tiskne jeden výsledek výpočtu.
(defun print-result (sub)
  (if (cdr sub)
      (print-sub sub)
    (format t "~%success ")))

;; Ptá se uživatele, zda pokračovat v hlednání výsledků.
(defun ask-for-next ()
  (format t "~%Next? (y - yes, n - no): ")
  (let ((char (read-char)))
    (eql char #\y)))
  
;; Interaktivně tiskne proud výsledků.
(defun print-results-stream (result-stream)
  (print-result (stream-car result-stream))
  (when (ask-for-next)
    (let ((rest (stream-cdr result-stream)))
      (cond
       (rest
        (print-results-stream rest))
       (t
        (format t "~%end "))))))

(setf dspec:*redefinition-action* :quiet)

;; Tiskne výsledky výpočtu.
(defun print-results (result-stream)
  (if result-stream
      (print-results-stream result-stream)
    (format t "fail")))

(setf dspec:*redefinition-action* :warn)
       
;; Před přidáním pravidla zakódujeme hodnoty.
(defun add-rule (rule)
  (setf *program* (append *program* (list (encode rule)))))

(setf dspec:*redefinition-action* :quiet)

(defmacro <- (head &rest body)
  `(add-rule '(<- ,head ,@body)))

(setf dspec:*redefinition-action* :warn)


#|
;; Test:
(let ((*program* '()))
  (<- (even 0))
  (<- (even (succ (succ ?x))) 
      (even ?x))
  (-> (even ?x)))
|#