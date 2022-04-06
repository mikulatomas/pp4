;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 0_6vector_8.lisp - Zásobníkové programování
;;;
;;; Jazyk pro myšlený zásobníkový stroj.
;;; fáze 8 - Vektory
;;;
;;; Předchozí soubory musejí být načteny.

;; Vektor je jednorozměrné pole.
;; Práce s vektory v Lispu:
;; Literál: #(el1 ... eln)
;; Funkce na vytvoření: make-array
;; Místo na prvek vektoru: (aref vector index)


;; Vektor je hodnota.
(defmethod exec-elem ((elem vector))
  (push elem *rslt*))

#|
(execute #(1 2 3))
|#

;; Slova na práci s vektory.
;; ( length -- vector)
(define-word :make-vector (lambda ()
                             (push
                              (make-array (list (pop *rslt*)))
                              *rslt*)))

;; ( vector index -- element)
(define-word :vector-ref (lambda ()
                           (let ((index (pop *rslt*)))
                             (push
                              (aref (pop *rslt*) index)
                              *rslt*))))


;; ( vector index value -- value)
(define-word :vector-set! (lambda ()
                            (let ((value (pop *rslt*))
                                  (index (pop *rslt*)))
                              (setf (aref (pop *rslt*) index)
                                    value)
                              (push value *rslt*))))


#|
(execute 3 :make-vector)
(execute #(1 2 3) 1 :vector-ref)
(execute #(1 2 3) :dup 1  4 :vector-set! :drop)
|#

;; Procedury na práci s vektory:
(execute :noexec '(%sub :vector-ref) :bind 'vector-ref)
(execute :noexec '(%sub :vector-set!) :bind 'vector-set!)
(execute :noexec '(%sub :make-vector) :bind 'make-vector)

#|
(execute '(vector-ref #(1 2 3) 1))

(execute '(let ((vect #(1 1 1)))
            (vector-set! vect 1 2)
            vect))

(execute '(let ((vect (make-vector 5)))
            (print vect)
            (dotimes (i 5)
              (vector-set! vect i i))
            (dotimes (i 5)
              (print (vector-ref vect i)))
            vect))
|#

;; Marko na pohodlné vytváření vektorů.
(defun vector-compile-set (vector args index)
  (when args
    `((vector-set! ,vector ,index ,(car args))
      ,@(vector-compile-set vector (cdr args) (+ index 1)))))

(defmethod compile-op ((op (eql 'vector)) args)
  (let ((vector (gensym "VECTOR")))
    (sub `(let ((,vector (make-vector ,(length args))))
            ,@(vector-compile-set vector args 0)
            ,vector))))

#|
(execute '(vector (+ 1 2) (* 2 2) 4))
|#
;; Logika

(execute '(define not (lambda (val)
                        (if val nil t))))

;; Makro and
(defmethod compile-op ((op (eql 'and)) args)
  (cond
   ((not args)
    (sub t))
   ((not (cdr args))
    (sub (car args)))
   (t
    (sub `(if ,(car args)
              (and ,@(cdr args))))))) 

#|
(execute '(and))
(execute '(and 1))
(execute '(and 1 2 3))
|#

;; Makro or
(defmethod compile-op ((op (eql 'or)) args)
  (cond
   ((not args)
    (sub nil))
   ((not (cdr args))
    (sub (car args)))
   (t
    (let ((result (gensym)))
    (sub 
     `(let ((,result ,(car args)))
        (if ,result
            ,result
          (or ,@(cdr args)))))))))

#|
(execute '(or))
(execute '(or 1))
(execute '(or  nil 1 2))
|#

;; Makro when
(defmethod compile-op ((op (eql 'when)) args)
  (sub `(if ,(car args)
            (progn ,@(cdr args))
          nil)))

;; Makro unless
(defmethod compile-op ((op (eql 'unless)) args)
  (sub `(if ,(car args)
            nil
            (progn ,@(cdr args)))))


;;; Aritmetika
;; (a b -- a modulo b)
(define-word :mod (lambda ()
                    (let ((val (pop *rslt*)))
                      (push
                       (mod (pop *rslt*) val)
                       *rslt*))))

;; (a b -- log. hodnota a <= b)
(define-word :<=
             (lambda () 
               (let ((arg (pop *rslt*)))
                 (push (<= (pop *rslt*) arg) *rslt*))))

;; (a b -- log. hodnota a > b)
(define-word :>
             (lambda () 
               (let ((arg (pop *rslt*)))
                 (push (> (pop *rslt*) arg) *rslt*))))

;; (a b -- log. hodnota a >= b)
(define-word :>=
             (lambda () 
               (let ((arg (pop *rslt*)))
                 (push (>= (pop *rslt*) arg) *rslt*))))


(execute :noexec '(%sub :mod) :bind 'mod)
(execute :noexec '(%sub :<=) :bind '<=)
(execute :noexec '(%sub :>) :bind '>)
(execute :noexec '(%sub :>=) :bind '>=)

;; Přidáme makra do Lispu pro lepší odsazení kódu Scheme:
;; (Samotná makra v Lispu používat nebudeme.)
(defmacro co-dotimes ((var count) &body body)
  `(dotimes (,var ,count)
     ,@body))

(defmacro while (condition &body body)
  `(loop
    (unless ,condition (return))
    ,@body))

