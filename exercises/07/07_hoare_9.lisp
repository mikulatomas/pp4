;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 07_hoare_9.lisp - Zásobníkové programování
;;;
;;; Jazyk pro myšlený zásobníkový stroj.
;;; fáze 9 - Hoareova logika
;;;
;;; Předchozí soubory musejí být načteny.

;; Globální proměnná *atomic* rozhoduje, zda je kód prováděn atomicky.
(defvar *atomic* nil)

;; Upravíme execute tak, aby nemohlo dojít k přerušení vykonávání
;; v případě, že je *atomic* Pravda.
(setf dspec:*redefinition-action* :quiet)

(defun execute (&rest code)
  (let ((*rslt* '())
   (*exec* (append code '(:exit)))
   (*time* 0)
   (*atomic* nil)
   (*rprs* '()))
    (loop 
     (when (and (null *exec*) (null *rprs*)) (return))
     ;; Při provádění složené atomické akce 
     ;; zastavíme čas.
     (unless *atomic*
       (if (= *time* 0)
           (scheduler)
         (decf *time*)))
     (if *exec*
         (exec-step)
       (scheduler)))
    (pop *rslt*)))

(setf dspec:*redefinition-action* :warn)

;;; Slova uvozující a ukončující atomický kód.
;; ( -- )
(define-word :atom-start (lambda ()
                           (setf *atomic* t)))
;; ( -- )
(define-word :atom-end (lambda ()
                         (setf *atomic* nil)))

#|
(execute 0 :bind 'n :atom-start 'n 1 :+ :set! 'n :atom-end 'n :unbind) 
|#

;; Speciální operátor atomic
;; Syntax:
;; (atomic . body)
;;
;; Tělo se vyhodnotí atomicky.
;; Atomické kódy není možné vnořovat.

(defmethod compile-op ((op (eql 'atomic)) args)
  (apply #'sub :atom-start (cons `(progn ,@args) '(:atom-end))))


#|
(execute '(let ((n 0))
            (co
             (dotimes (i 100)
               (atomic (set! n (+ n 1))))
             (dotimes (i 100)
               (atomic (set! n (+ n 1)))))
            n))
|#


;; Generování náhodných čísel.
(define-word :random (lambda () ; ( num -- náhdoné číslo do `num`)
                       (push (random (pop *rslt*)) *rslt*)))

#|
(execute 10 :random)
|#

;; Pozastavení běhu programu.
(define-word :delay ; (n --)
             '(:dup 0 := :if 1 :- :delay :else :drop :then))

#|
(execute 100000 :delay)
|#


;; Slovo na rozpoznání atomického kódu.
(define-word :atomic? (lambda () ; ( -- pravdivostní hodnota zda je kód atomický )
                        (push *atomic* *rslt*)))

;; Rozšíříme si makro await:
;; (await condition . body)
;; 
;; Makro opakovaně vyhodnocuje podmínku condition.
;; Pokud je podmínka splněna, vyhodnotí se tělo.
;;
;; V případě, že se kód provádí atomicky,
;; se atomicky vyhodnocí podmínka a pokud je splněna i tělo.
;; Při nesplnění podmínky se na chvíli atomičnost přeruší,
;; aby ostatní procesy dostaly šanci hodnotu podmínky změnit. 

(setf dspec:*redefinition-action* :quiet)

(defmethod compile-op ((op (eql 'await)) args)
  (sub :noexec `(%sub ,(car args) :if 
                      ;; Pokud je kód prováděn atomicky,
                      ;; na náhodný čas atomičnost přerušíme.
                      :atomic? :if :else :atom-end 100 :random :delay ::atom-start :then t :else 
                      (progn ,@(cdr args)) :drop nil :then) 
       :loop nil))

(setf dspec:*redefinition-action* :warn)

;; Pouze zlepšuje odsazení makra await ve Scheme.
(defmacro await (con &body body)
  `(progn (loop
           (when ,con 
             (return)))
     ,@body))

#|
;; Nikdy nevytiskne záporné číslo:
(execute '(let ((n 0))
            (co
             (dotimes (i 100)
               (atomic (await (< 0 n)
                         (set! n (- n 1))
                         (print n))))
             (dotimes (i 100)
               (atomic (set! n (+ n 1)))))))
|#

;; Přidáme syntax: [expr1 ... exprn]
;; Výrazy v hranatých závorkách se vyhodnotí atomicky.

(defun right-paren-reader (stream char)
  (declare (ignore stream))
  (error "Non-balanced ~s encountered." char))

(defun left-brack-reader (stream char)
  (declare (ignore char))
  `(atomic ,@(read-delimited-list #\] stream t)))

(set-macro-character #\[ #'left-brack-reader) 
(set-macro-character #\] #'right-paren-reader)

#|
'[(set! n (+ n 1)) (print n)]

(execute '(let ((n 0))
            (co
             (dotimes (i 100)
               [(set! n (+ n 1))])
             (dotimes (i 100)
               [(set! n (+ n 1))]))
            n))

(execute '(let ((n 0))
            (co
             (dotimes (i 100)
               [(await (< 0 n) (set! n (- n 1)) (print n))])
             (dotimes (i 100)
               [(set! n (+ n 1))]))))
|#


;; Makro assert
;; Syntax: (assert cond)
;;
;; Atomicky se provede následující.
;; 1. Vyhodnotí podmínku cond.
;; 2. Pokud není podmínka splněna, vytiskne informaci o jejím nesplnění.
;;
;; Nesmí být použito v atomickém kódu.

(defmethod compile-op ((op (eql 'assert)) args)
  (sub `(atomic (unless ,(car args)
                  (print ,(vector 'assertion-failed (car args)))))))

#|
(execute '(let ((n 0))
            (co
             (dotimes (i 100)
               [(set! n (- n 1))]
               (assert (< 0 n)))
             (dotimes (i 100)
               [(set! n (+ n 1))]))))
             
|#

;; Přidáme syntax: {cond}
;; Výraz je ekvivalentní výrazu (assert cond) 

(defun left-brace-reader (stream char)
  (declare (ignore char))
  `(assert ,(car (read-delimited-list #\} stream t))))

(set-macro-character #\{ #'left-brace-reader) 
(set-macro-character #\} #'right-paren-reader)

;; Hack, aby editor rozuměl hranatým a složeným závorkám
(editor::set-vector-value
 (slot-value editor::*default-syntax-table* 'editor::table) '(#\[ #\{) 2)
(editor::set-vector-value
 (slot-value editor::*default-syntax-table* 'editor::table) '(#\] #\}) 3)


#|
(execute '(let ((n 0))
            (co
             (dotimes (i 100)
               [(set! n (- n 1))]
               {(<= 0 n)})
             (dotimes (i 100)
               [(set! n (+ n 1))]))))

(execute '(let ((n 0))
            (co
             (dotimes (i 100)
               [(await (< 0 n) (set! n (- n 1)))]
               {(<= 0 n)})
             (dotimes (i 100)
               [(set! n (+ n 1))]))))
|#