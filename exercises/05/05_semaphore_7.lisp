;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 05_semaphore_7.lisp - Zásobníkové programování
;;;
;;; Jazyk pro myšlený zásobníkový stroj.
;;; fáze 7 - Semafory
;;;
;;; Předchozí soubory musejí být načteny.


;; Slovo na ukončení činnosti stroje
(define-word :exit
             (lambda ()
               (setf *exec* nil
                     *rprs* nil)))


#|
;; Kód za :exit se nevykoná: 
(execute 1 :print :exit 2 :print)
|#

;; Přidání automatického ukončení hlavního procesu:
(setf dspec:*redefinition-action* :quiet)

(defun execute (&rest code)
  (let ((*rslt* '())
   (*exec* (append code '(:exit)))
   (*time* 0)
   (*rprs* '()))
    (loop 
     (when (and (null *exec*) (null *rprs*)) (return)) 
     (if (= *time* 0)
         (scheduler)
       (decf *time*))
     (if *exec*
         (exec-step)
       (scheduler)))
    (pop *rslt*)))


(setf dspec:*redefinition-action* :warn)


#|

Semafor je reprezentován seznamem:

(%sem value . blocked)

kde value je nezáporné celé číslo a blocked je seznam procesů (pokračováních).
|#

;; Semafor je hodnota:
(defmethod exec-list ((op (eql '%sem)) args)
  (push (cons '%sem args) *rslt*))

;; Pomocné funkce:
(defun take-running-process ()
  (prog1 
      `(%cont ,*rslt* ,*exec* ,*bnd*)
    (setf *exec* nil)))

(defun take-blocked-process (semaphore)
  (let ((process (nth (random (length (cddr semaphore))) (cddr semaphore))))
    (setf (cddr semaphore) (remove process (cddr semaphore)))
    process))
    

;; Vytvoření semaforu:
(define-word :sem
             (lambda ()
               (let ((value (pop *rslt*)))
                 (unless (<= 0 value)
                   (error "Value can not be negative"))
                 (push `(%sem ,value)
                       *rslt*))))


;; Nizozemsky: passering (projíždějící),
;; Česky: počkat
(define-word :p
             (lambda ()
               (let ((semaphore (pop *rslt*)))
                 (cond
                  ((plusp (cadr semaphore))
                   (decf (cadr semaphore)))
                  (t
                   (push (take-running-process) (cddr semaphore)))))))


;; Nizozemsky: vrijgave (uvolni)
;; Česky: vyhlásit
(define-word :v
             (lambda ()
               (let ((semaphore (pop *rslt*)))
                 (if (null (cddr semaphore))
                     (incf (cadr semaphore))
                   (push (take-blocked-process semaphore) *rprs*)))))


#|
(execute 0 :sem
         :dup
         :noexec '(%sub 1 :print :drop :v) :swap :process-exec
         :p
         2 :print)
|#

#|
Deadlock:
(execute 0 :sem :p 2 :print)
|#

#|
(execute 0 :sem :bind 'sem
         :noexec '(%sub 1 :print sem :v) nil :process-exec
         'sem :p
         2 :print
         :unbind)

|#

;; Procedury na práci se semafory:
(execute :noexec '(%sub :sem) :bind 'make-semaphore)
(execute :noexec '(%sub :p nil) :bind 'semaphore-wait)
(execute :noexec '(%sub :v nil) :bind 'semaphore-signal)

#|
(execute '(let ((s (make-semaphore 0)))
            (co
             (progn
               (print 1)
               (semaphore-signal s))
             (progn
               (semaphore-wait s)
               (print 2)))))


;; Neošetřená kritická sekce:
(execute '(let ((n 0))
            (co
             (dotimes (i 100)
               (set! n (+ n 1)))
             (dotimes (i 100)
               (set! n (+ n 1))))
            n))


;; Použití semaforu k ošetření kritické sekce:
(execute '(let ((n 0)
                (sem (make-semaphore 1)))
            (co
             (dotimes (i 100)
               (semaphore-wait sem)
               (set! n (+ n 1))
               (semaphore-signal sem))
             (dotimes (i 100)
               (semaphore-wait sem)
               (set! n (+ n 1)) 
               (semaphore-signal sem)))
         n))
|#


;; Marko co změníme tak, aby čekání
;; na ukončení vytvořených procesů používalo semafory.

(setf dspec:*redefinition-action* :quiet)

(defmethod compile-op ((op (eql 'co)) args)
  (if args
      (let ((end (gensym "END"))
            (arg (gensym "ARG")))
        (sub 
         `(let ((,end (make-semaphore 0)))
            (process-run-function (lambda (,arg)
                                    ,(car args)
                                    (semaphore-signal ,end))
                                  nil)
            (co ,@(cdr args))
            (semaphore-wait ,end))))
    (sub nil)))

(setf dspec:*redefinition-action* :warn)

#|
Test:
(execute '(progn
           (co
            (dotimes (i 10)
              (print 1))
            (dotimes (i 10)
              (print 2)))
           (print 3)))

;; Vysvětlete:
(execute '(let ((sem (make-semaphore 0)))
            (co
             (progn
               (semaphore-wait sem)
               (print 1)))
            (print 2)))
|#


;; Procedura na vytvoření zámku:
(execute '(define make-lock (lambda ()
                              (make-semaphore 1))))


;; Makro with-lock
;;
;; Syntax: (with-lock (lock) . body)
;;
;; Vyhodnotí tělo za použití zámku lock. 
;; Vrátí hodnotu posledního výrazu těla.

(defmethod compile-op ((op (eql 'with-lock)) args)
  (let ((lock (caar args))
        (body (cdr args))
        (lock-sym (gensym "LOCK"))
        (res-sym (gensym "RES")))
    (sub `(let ((,lock-sym ,lock))
            (semaphore-wait ,lock-sym)
            (let ((,res-sym (progn ,@body)))
              (semaphore-signal ,lock-sym)
              ,res-sym)))))


#|
;; Použití zámku k ošetření kritické sekce:
(execute '(let ((n 0)
                (lock (make-lock)))
            (co
               (dotimes (i 100)
                 (with-lock (lock)
                   (set! n (+ n 1))))
               (dotimes (i 100)
                 (with-lock (lock)
                   (set! n (+ n 1)))))
            n))
|#


;; Makro co-dotimes
;; Syntax:
;;   (co-dotimes (var form) . body)
;;
;; Funguje podobně jako makro dotimes, ale každou iteraci vyhodnocuje paralelně
;; v různých procesech. Čeká, až všechny procesy skončí.

(defmethod compile-op ((op (eql 'co-dotimes)) args)
  (let ((var (caar args))
        (count-form (cadar args))
        (body (cdr args))
        (fun (gensym "FUN"))
        ;; Semaforem zařídíme čekání na skončení procesů:
        (sem (gensym "SEM"))
        (count (gensym "COUNT")))
    (sub 
     `(let ((,sem (make-semaphore 0))
            ;; Funkce, která se bude volat paralelně:
            (,fun (lambda (,var) ,@body (semaphore-signal ,sem)))
            (,count ,count-form))
        ;; Spuštění procesů:
        (dotimes (i ,count)
          (process-run-function ,fun i))
        ;; Čekání, až procesy skončí:
        (dotimes (i ,count)
          (semaphore-wait ,sem))))))

#|
;; Vytiskne paralelně čísla od nuly po deset:
(execute '(co-dotimes (i 10)
                      (print i)))


;; V deseti procesech stokrát inkrementujeme počítadlo:
(execute '(let ((n 0)
                (lock (make-lock)))
            (co-dotimes (i 10)
                        (dotimes (j 100)
                          (with-lock (lock)
                            (set! n (+ n 1)))))
            n))
|#


