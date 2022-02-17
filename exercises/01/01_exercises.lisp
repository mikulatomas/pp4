;; Pravdivostní hodnoty
(execute '(define t 't))
(execute '(define nil 'nil))

;; -------
;; 1. let - doplňte tělo funkce compile-op
;; -------

(defmethod compile-op ((op (eql 'let)) args)
    ...)

#|
(execute '(let ((a 2)) 
            (print (+ a 1))))



(execute '(let ((a 1) 
                (b 2)) 
            (print (+ a b))))

(execute '(let ((a 1))
            (print a)
            (set! a 2)
            (print a)))
|#

;; -------
;; 2. cyklus loop
;; -------
;; 
;; Na vrcholu zásobníku exec musí být prvek, který po vykonání nechá na zásobníku jedinou hodnotu.
;;
;; Vykonává vrchní prvek zásobníku exec, dokud na vrcholu zásobníku exec zůstává Pravda. 

;; Definujte zásobníkové slovo :loop pomoci funkce define-word
(define-word :loop ...)

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

;; -------
;; 3. while - doplňte tělo funkce compile-op
;; -------

(defmethod compile-op ((op (eql 'while)) args)
  ...)


#|
(execute '(let ((a 0)) 
            (while (< a 10)  
                   (print a) 
                   (set! a (+ a 1)))))
|#

;; -------
;; 4. dotimes - doplňte tělo funkce compile-op
;; -------

(defmethod compile-op ((op (eql 'dotimes)) args)
  ...)

#|
(execute '(dotimes (i 10) 
            (print i)))
|#

