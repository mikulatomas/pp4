;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 03_stacks_6.lisp - Zásobníkové programování
;;;
;;; Jazyk pro myšlený zásobníkový stroj.
;;; fáze 6 - Paralelní scheme
;;;
;;; Předchozí soubory musejí být načteny.

#|
; Chceme vyjádřit v Scheme:
(execute nil :bind 'end?
         :noexec '(%sub :print t :set! end?) 1 :process-exec 
         :noexec '(%sub end?) :await 
         :unbind)
|#


;; process-run-function
;; Očekává proceduru jednoho parametru a hodnotu.
;; V novém procesu aplikuje proceduru na hodnotu.
;; Nový proces bude mít vazby všech symbolů, které existují v momentě volání funkce.
(execute :noexec '(%sub :process-exec nil) :bind 'process-run-function)

#|
(execute nil :bind 'end?
         '(process-run-function (lambda (v) 
                                 (print v) 
                                 (set! end? t)) 1)
         
         :noexec 'end? :await
         :unbind)
|#

;; Speciální operátor await
(defmethod compile-op ((op (eql 'await)) args)
  (sub :noexec (car args) :await nil))

#|
(execute nil :bind 'end?
         '(process-run-function (lambda (v) 
                                 (print v) 
                                 (set! end? t)) 1)
         '(await end?)
         :unbind)
|#

;; makro let
(defmethod compile-op ((op (eql 'let)) args)
  (let ((bindings (car args))
        (body (cdr args)))
    (let ((symbols (mapcar #'first bindings))
          (exprs (mapcar #'second bindings)))
      (sub `((lambda ,symbols ,@body) ,@exprs)))))

#|
(compile-scheme '(let ((a 2) (b 3)) (+ a b)))

(execute '(let ((a 2)) 
            (print (+ a 1))))

(execute '(let ((a 1) 
                (b 2)) 
            (print (+ a b))))

(execute '(let ((a 1))
            (print a)
            (set! a 2)
            (print a)))

(execute '(let ((end? nil)) 
            (process-run-function (lambda (v) 
                                    (print v) 
                                    (set! end? t)) 1)
            (await end?)))

(execute '(let ((n 0)
                (end1? nil)
                (end2? nil))
            (process-run-function (lambda (arg) 
                                    (set! n 1)
                                    (set! end1? t)) 
                                  nil)
            (process-run-function (lambda (arg)
                                    (set! n 2)
                                    (set! end2? t))
                                  nil)
            (await end1?)
            (await end2?)
            (print n)))
|#

;; makro co
(defmethod compile-op ((op (eql 'co)) args)
  (if args
      (let ((endp (gensym "ENDP"))
            (arg (gensym "ARG")))
        (sub 
         `(let ((,endp nil))
            (process-run-function (lambda (,arg)
                                    ,(car args)
                                    (set! ,endp t))
                                  nil)
            (co ,@(cdr args))
            (await ,endp))))
    (sub nil)))

#|
(compile-scheme '(co (print 1) (print 2)))

(execute '(let ((n 0))
            (co
             (set! n 1)
             (set! n 2))
            (print n)))

(execute '(let ((n 0))
            (co
             (let ((tmp nil))
               (set! tmp (+ n 1))
               (set! n tmp))
             (let ((tmp nil))
               (set! tmp (+ n 1))
               (set! n tmp)))
            (print n)))

|#


;; makro co-progn
;; (co-progn . body)
;; vyhodnotí paralelně výrazy v body a vrátí hodnotu posledního výrazu.
(defmethod compile-op ((op (eql 'co-progn)) args)
  (if args
      (let ((res (gensym "RES")))
        (sub `(let ((,res nil))
                 (co ,@(butlast args)
                     (set! ,res ,@(last args)))
                 ,res)))
    (sub nil)))

#|
(compile-scheme '(co-progn (print 1) (print 2)))

(execute '(let ((n 0))
            (co-progn
             (set! n 1)
             n)))

|#


;; Cyklus
;; 
;; Na vrcholu zásobníku exec musí být prvek, který po vykonání nechá na zásobníku jedinou hodnotu.
;;
;; Vykonává vrchní prvek zásobníku exec, dokud na vrcholu zásobníku exec zůstává Pravda. 
(define-word :loop '(:dup :exec :if :drop :else :loop :then))

#|

;; Nekonečný cyklus, který tiskne samé jedničky:
(execute :noexec '(%sub 1 :print) :loop)

|#
;; (a b -- log. hodnota a < b)
(define-word :<
             (lambda () 
               (let ((arg (pop *rslt*)))
                 (push (< (pop *rslt*) arg) *rslt*))))


#|
;; Tisk čísel od nuly po devítku včetně:
(execute 0 :bind 'n 
         :noexec '(%sub n 10 :< :dup :if :else n :print 1 :+ :set! n :then) 
         :loop 
         :unbind)
|#

;; Definice podprogramu ostrou nerovnost:
(execute :noexec '(%sub :<) :bind '<)

#|
(execute '(< 1 2))
|#

;; speciální operátor while
(defmethod compile-op ((op (eql 'while)) args)
  (sub :noexec `(%sub ,(car args) :dup :exec :if :else (progn ,@(cdr args)) :drop :then) :loop nil))


#|
(execute '(let ((a 0)) 
            (while (< a 10)  
                   (print a) 
                   (set! a (+ a 1)))))
|#



;; makro dotimes
(defmethod compile-op ((op (eql 'dotimes)) args)
  (let ((var (caar args))
        (count-form (cadar args))
        (body (cdr args)))
    (let ((sym-count (gensym "COUNT")))
      (sub `(let ((,var 0)
                  (,sym-count ,count-form))
              (while (< ,var ,sym-count)
                     ,@body
                     (set! ,var (+ ,var 1))))))))

#|
(execute '(dotimes (i 10) 
            (print i)))
|#


;; Závěrečný test
#|
;; Jaké hodnoty může výraz vrátit?
(execute '(let ((n 0))
            (co
             (dotimes (i 100)
               (set! n (+ n 1)))
             (dotimes (i 100)
               (set! n (+ n 1))))
            n))

;; Přepsáno na atomické příkazy:
(execute '(let ((n 0))
            (co
             (let ((tmp nil))
               (dotimes (i 100)
                 (set! tmp (+ n 1)) 
                 (set! n tmp)))
             (let ((tmp nil))
               (dotimes (i 100)
                 (set! tmp (+ n 1)) 
                 (set! n tmp))))
            n))
|#





