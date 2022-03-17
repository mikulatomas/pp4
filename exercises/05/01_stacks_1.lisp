;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 01_stacks_1.lisp - Zásobníkové programování
;;;
;;; Jazyk pro myšlený zásobníkový stroj.
;;; fáze 1 - základy
;;;

;;
;; Hlavní zásobníky
;;

;; Datový zásobník (Result Stack) bude v proměnné *rslt*
;; Programový zásobník (Execution Stack) bude v proměnné *exec*
;; Kvůli pohodlnějšímu ladění budou zastíněny v hlavní funkci execute.
;; (Aby byly zásobníky snadno vidět ve stepperu.)
 
(defvar *rslt*)
(defvar *exec*)

;; Slovník je zásobník párů (klíč . hodnota)
;; práce se slovníky:

(defun dict-find (key dict)
  (let ((pair (find key dict :key #'car)))
    (unless pair (error "Key not found in dictionary: ~s" key))
    (cdr pair)))

(defmacro dict-push (key val dict)
  `(push (cons ,key ,val) ,dict))

(defun dict-set (key value dict)
  (let ((pair (find key dict :key #'car)))
    (unless pair (error "Key not found in dictionary: ~s" key))
    (setf (cdr pair) value)))

;; Slovník definic slov. Každá definice je pár (slovo . kód).
;; Kód je buď lispová funkce (vestavěné slovo), nebo kód našeho jazyka
;; v seznamu (uživatelsky definované slovo).

(defvar *word*)

;; 
;; Slova
;;

;; Slova jsou lispovské klíče
(defun wordp (el)
  (keywordp el))

(defun word-code (w)
  (dict-find w *word*))

;; Vykonání slova (je-li na vrcholu programového zásobníku)
(defun exec-word (word)
  (let ((code (word-code word))) 
    (if (functionp code)
        (built-in-exec code)
      (user-exec code))))

(defun built-in-exec (fun)
  (funcall fun))

(defun user-exec (uc)
  (setf *exec* (append uc *exec*)))

;;
;; Základní vestavěná slova:
;;

(setf *word*
      (list ;; (a b -- součet a + b)
            (cons :+ 
                  (lambda () 
                    (push (+ (pop *rslt*) (pop *rslt*)) *rslt*))) 
            ;; (a b -- rozdíl a - b)
            (cons :-
                  (lambda ()
                    (let ((arg (pop *rslt*)))
                      (push (- (pop *rslt*) arg) *rslt*))))
            ;; (a b -- součin a * b)
            (cons :* 
                  (lambda () 
                       (push (* (pop *rslt*) (pop *rslt*)) 
                             *rslt*)))
            ;; (a b -- podíl a / b)
            (cons :/ 
                  (lambda ()
                    (let ((arg (pop *rslt*)))
                      (push (/ (pop *rslt*) arg) *rslt*))))
            ;; (a b -- log. hodnota a = b)
            (cons := 
                  (lambda () 
                    (push (= (pop *rslt*) (pop *rslt*)) *rslt*)))))

;; Další vytvoříme pomocí funkce define-word
(defun define-word (name code)
  (dict-push name code *word*))

;; (a b -- b a) 
(define-word :swap (lambda ()
                     (setf *rslt* (append (list (second *rslt*)
                                                (first *rslt*))
                                          (cddr *rslt*)))))

;; (a b c -- b c a) 
(define-word :rot (lambda ()
                    (setf *rslt* (append (list (third *rslt*)
                                               (first *rslt*)
                                               (second *rslt*))
                                         (cdddr *rslt*)))))

;; (a -- )
(define-word :drop (lambda ()
                     (pop *rslt*)))

;; (a -- a a)
(define-word :dup (lambda ()
                    (push (car *rslt*) *rslt*)))

;; (a b -- a b a)
(define-word :over (lambda ()
                     (push (second *rslt*) *rslt*)))


;; (a -- a)
(define-word :print (lambda () (print (car *rslt*)))) 

;; Slovo :if pracuje tak, že upravuje vrchol zásobníku exec

(defun tail (elem stack)
  "Vrací pokračování seznamu stack začínající prvkem elem. Přeskakuje bloky :if-:else-:then"
  (cond ((null stack) (error "Empty stack. "))
        ((eql (car stack) elem) stack)
        ((eql (car stack) :if) (tail elem (cdr (tail :then (cdr stack)))))
        (t (tail elem (cdr stack)))))

#|
(tail :then '( 1 2 :if 3 4 :else 5 6 :then 7 8 :else :if 9 10 :else 11 12 :then 13 :then 14 15))
(tail :else '( 1 2 :if 3 4 :else 5 6 :then 7 8 :else :if 9 10 :else 11 12 :then 13 :then 14 15))
|#

(defun stack-if (val stack)
  (let* ((else-tail (tail :else stack))
         (then-tail (tail :then else-tail)))
    (append (if val 
                (ldiff (cdr else-tail) then-tail)
              (ldiff stack else-tail))
            (cdr then-tail))))

#|
(stack-if t '(1 2 :+ :else 3 4 :+ :then a b))
(stack-if nil '(1 2 :+ :else 3 4 :+ :then a b))
|#

(define-word :if (lambda ()
                   (setf *exec* (stack-if (pop *rslt*) *exec*))))
                       
;;
;; Spuštění programu.
;;
;; Jeden krok

(defun exec-step ()
  (exec-elem (pop *exec*)))

(defmethod exec-elem (elem)
  (error "Unknown element on exec stack: ~s" elem))

(defmethod exec-elem ((elem number))
  (push elem *rslt*))

(defmethod exec-elem ((elem string))
  (push elem *rslt*))


(defmethod exec-elem ((elem symbol))
  (if (wordp elem)
      (exec-word elem)
    (call-next-method)))




;; Celý program
;; Tady vytváříme vazby proměnných *rslt* a *exec*.
;; Vazby budou dynamické.
(defun execute (&rest code)
  (let ((*rslt* '())
        (*exec* code))
    (loop (when (null *exec*) (return))
          (exec-step))
    (pop *rslt*)))

#|

;; Výrazy je dobré vyhodnocovat v Listeneru a ne tady přes F8.
;; Když se rekurzivní výraz zacyklí, je pak snadné ho zastavit tlačítkem Break.
;; Vždy je dobré dělat to přes breakpoint, jak jsem ukazoval na přednášce.
;; Tak vidíte, jak se postupně vykonávají jednotlivé kroky výpočtu
;; a můžete sledovat stav obou zásobníků.

(execute 1)
(execute 3 2 :- 4 :*)

(execute 1 2 :swap :drop)

;; Výpočet 4*(4 + 5):
(execute 4 5 :over :+ :*)

(execute 0 1 := :if 2 5 :* :else 2 10 :* :then)
(execute 1 1 := :if 2 5 :* :else 2 10 :* :then)

;; Tohle povede k chybě, protože symbol na zásobník s kódem (zatím) nepatří:
(execute 'a)

(define-word :fact
             '(:dup 0 := :if :dup 1 :- :fact :* :else
                             :drop 1 :then))

(execute 5 :fact)

;; Iterativní faktoriál. Porovnejte, co se děje se zásobníkem proti
;; předchozí verzi.

;; pomocné slovo :factiter (vzpomeňte si, že u iterativní rekurze jsme
;; obvykle potřebovali napsat pomocnou funkci)
;; Na vrcholu zásobníku očekává už vypočítaný součin, pod ním číslo
;; udávající, kolikrát je ještě třeba násobit.
(define-word :factiter '(:over :dup 1 := :if :* :swap 1 :- :swap :factiter :else
                                             :drop :swap :drop :then))

(define-word :ifact '(1 :factiter))

(execute 5 :ifact)

|#

;; Šikovná pomůcka (a ukázka uživatelsky definovaného slova): 
;; rotace opačným směrem.
(define-word :rot- '(:rot :rot ))


#|

;; n-té Fibonacciho číslo počínaje n = 1

(define-word :fib '(0 1 :rot :xfib))

(define-word :xfib
             '(:dup 1 := :if :rot- :dup :rot :+ :rot 1 :- :xfib :else
                             :drop :swap :drop :then))


(execute 5 :fib)

;; tisk
(execute 5 :print)
|#




