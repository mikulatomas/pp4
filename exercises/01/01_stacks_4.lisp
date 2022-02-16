;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 01_stacks_4.lisp - Zásobníkové programování
;;;
;;; Jazyk pro myšlený zásobníkový stroj.
;;; fáze 4 - interpretace vloženého Scheme
;;;
;;; Předchozí soubory musí být načteny.


#|
Do programu pro náš stroj bude možné vkládat výrazy jazyka Scheme.
Ty budeme za běhu programu překládat do jazyka stroje a vykonávat.
(Primitivní Just In Time Compilation, kterou známe z Javy, .NET atd.)
|#

;; Atomické výrazy Scheme jsou tytéž jako hodnoty našeho jazyka:
;; čísla a symboly (které nejsou slova).
;; Složený výraz Scheme je neprázdný seznam, který není podprogram

;; Můžeme tedy změnit naši funkci exec-list:
(setf dspec:*redefinition-action* :quiet)

(defmethod exec-list (op args)
  (compile-and-push (cons op args)))

(setf dspec:*redefinition-action* :warn)


;; Účelem tohoto souboru je definovat funkci compile-scheme
;; Zavedeme čtyři spec. operátory (jako v prosinci)

;; Pomocná funke na vytvoření podprogramu s daným kódem
(defun sub (&rest code)
  (cons '%sub code))

;; Kompilace schemového výrazu do podprogramu
;; a jeho uložení na programový zásobník
(defun compile-and-push (expr)
  (push (compile-scheme expr) *exec*))

;; Kompilace schemového výrazu do podprogramu
;; Funkce je čistě funkcionální. Na vstupu přijímá 
;; výraz ve Scheme, vrací podprogram zásobníkového 
;; jazyka (obvykle proceduru). 
(defun compile-scheme (expr)
  (compile-op (car expr) (cdr expr)))

;; Vlastní kompilace je rozdělena podle operátorů.

(defmethod compile-op ((op (eql 'quote)) args)
  (sub :noexec (car args)))

#|
(compile-scheme '(quote abc))
|#

;; Podvýrazy se zkompilují příště
(defmethod compile-op ((op (eql 'if)) args)
  (let ((condition (first args))
        (then (second args))
        (else (third args)))
    (sub condition :if else :else then :then)))

#|
(compile-scheme '(if a b c))
|#

(defmethod compile-op ((op (eql 'define)) args)
  (let ((var (first args))
        (expr (second args)))
    (sub :noexec var expr :bind var)))

#|

(compile-scheme '(define a (+ 1 1)))

|#

;; Jednoduché lambda-výrazy mohou mít v těle jen jeden výraz.
;; Obecný lambda-výraz zavedeme níže.
;; U jednoduchých lambda-výrazu bez parametrů vytvoříme nový podprogram a pomocí 
;; :noexec potlačíme jeho spuštění. 
;; U výrazu s parametry vytvoříme lambda-výraz bez prvního parametru
;; s upraveným tělem.
(defmethod compile-op ((op (eql 'simple-lambda)) args)
  (let ((params (first args))
        (body-expr (second args)))
    (if (null params)
        (sub :noexec body-expr)
      (sub `(simple-lambda ,(cdr params)
              ,(sub :bind (car params)
                    body-expr
                    :unbind))))))

#|

(compile-op 'simple-lambda '((x y) (+ x y)))
(compile-op 'simple-lambda '(() (+ x y)))
(compile-scheme '(simple-lambda (y) (+ x y)))

|#

(defmethod compile-op ((op (eql 'progn)) args)
  (cond
   ((null args)
    (sub nil))
   ((null (cdr args))
    (sub (car args)))
   (t
    (sub (car args) :drop
          `(progn ,@(cdr args))))))
   
#|
(compile-op 'progn '())

(compile-op 'progn '((print 1)))

(compile-scheme '(progn 
                   (print 1)
                   (print 2)))
|#

;; Lambda-výrazy, které mohou mít v těle více výrazů,
;; vytvoříme pomocí progn a jednoduchých lambda-výrazů.
(defmethod compile-op ((op (eql 'lambda)) args)
  (sub `(simple-lambda ,(car args)
          (progn ,@(cdr args)))))

#|
(compile-scheme '(lambda (y) (print y) 1))
|#


(defmethod compile-op ((op (eql 'set!)) args)
  (let ((var (first args))
        (expr (second args)))
    (sub expr :set! var var)))

#|
(compile-scheme '(set! a (+ 3 3)))
|#

(defmethod compile-op (op args)
  (sub (apply #'sub args)
        op
        :exec))

#|

(compile-scheme '(a b c d))
|#


#|

;; Složitější testy

;; quote

(execute '(quote a))
;;umíte vysvětlit výsledek tohoto?
(execute '(1 2 3))
(execute ''(1 2 3))


;; if:

(execute '(if 't 1 2))
(execute '(if 'nil 1 2))

;; define

(execute '(define five 5))
(execute 'five)
(execute '(define t 't))
(execute '(define nil 'nil))

(execute '(if t 1 2))
(execute '(if nil 1 2))

;; lambda

(execute '(lambda () 1))
(execute '(lambda (a) 1))
(execute '((lambda () 1)))
(execute '((lambda (a) a) 2))
(execute '((lambda (a b) b) 3 4))
(execute '((lambda (a b) (print a) (print b)) 3 4))

;; progn

(execute '(progn 
            (print 1) 
            (print 2)))

;; aplikace:

(execute '(- 1 2))
(execute '(if (= 1 2) 3 4))

;; set!

(execute '((lambda (a)
             (print a)
             (set! a 5)
             (print a))
           2))

;; Zátěžový test:
(execute '(define fact (lambda (n)
                         (if (= n 0)
                             1
                           (* n (fact (- n 1)))))))

(execute '(fact 5))

|#


