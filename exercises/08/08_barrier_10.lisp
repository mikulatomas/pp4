;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 09_hoare_10.lisp - Zásobníkové programování
;;;
;;; Jazyk pro myšlený zásobníkový stroj.
;;; fáze 9 - Bariéry
;;;
;;; Předchozí soubory musejí být načteny.


;; Přejmenujeme původní co-dotimes na co-dotimes-basic
(defmethod compile-op ((op (eql 'co-dotimes-basic)) args)
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
(execute '(let ((n 5))
            (let ((turnstile (make-semaphore 0))
                  (turnstile2 (make-semaphore 1))
                  (lock (make-lock))
                  (count 0))
              (let ((barrier (lambda ()
                              (with-lock (lock)
                                (set! count (+ count 1))
                                (when (= count n)
                                  (semaphore-wait turnstile2)
                                  (semaphore-signal turnstile)))
                              (semaphore-wait turnstile)
                              (semaphore-signal turnstile)
                              (with-lock (lock)
                                (set! count (- count 1))
                                (when (= count 0)
                                  (semaphore-wait turnstile)
                                  (semaphore-signal turnstile2)))
                              (semaphore-wait turnstile2)
                              (semaphore-signal turnstile2))))
            (co-dotimes-basic (i 5)
                              (dotimes (j 10)
                                (print (vector i j))
                                (barrier)))))))
              
|#

;; Změníme co-dotimes tak, aby se procesy mohly synchronizovat funkcí barriere implementující bariéru.
(setf dspec:*redefinition-action* :quiet)

(defmethod compile-op ((op (eql 'co-dotimes)) args)
  (let ((var (caar args))
        (count-form (cadar args))
        (body (cdr args))
        (turnstile (gensym "TURNSTILE"))
        (turnstile2 (gensym "TURNSTILE2"))
        (lock (gensym "LOCK"))
        (count (gensym "COUNT"))
        (barrier-count (gensym "BARRIER-COUNT")))
    (sub 
     `(let ((,count ,count-form)
            (,turnstile (make-semaphore 0))
            (,turnstile2 (make-semaphore 1))
            (,lock (make-lock))
            (,barrier-count 0))
          (let ((barrier (lambda ()
                           (with-lock (,lock)
                             (set! ,barrier-count (+ ,barrier-count 1))
                             (when (= ,barrier-count ,count)
                               (semaphore-wait ,turnstile2)
                               (semaphore-signal ,turnstile)))
                           (semaphore-wait ,turnstile)
                           (semaphore-signal ,turnstile)
                           (with-lock (,lock)
                             (set! ,barrier-count (- ,barrier-count 1))
                             (when (= ,barrier-count 0)
                               (semaphore-wait ,turnstile)
                               (semaphore-signal ,turnstile2)))
                           (semaphore-wait ,turnstile2)
                           (semaphore-signal ,turnstile2))))
            (co-dotimes-basic (,var ,count) ,@body))))))

(setf dspec:*redefinition-action* :warn)

#|
(execute '(co-dotimes (i 5)
            (dotimes (j 10)
              (print (vector i j))
              (barrier))))
|#


