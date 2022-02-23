;; -------
;; 1. Jeden proces něco spočítá 1 + 1 a druhý vytiskne výsledek
;; -------

(execute ...)

;; -------
;; 2. Co se vytiskne? Vysvětlete.
;; -------

(execute nil :bind 'end1?
         nil :bind 'end2?
         0 :bind 'val
         :noexec '(%sub val 1 :+ :set! val t :set! end1?) nil :process-exec
         :noexec '(%sub val 1 :+ :set! val t :set! end2?) nil :process-exec
         :noexec '(%sub end1?) :await
         :noexec '(%sub end2?) :await
         'val :print
         :unbind
         :unbind
         :unbind)


;; -------
;; 3. Co se vytiskne? Vysvětlete.
;; -------

(define-word :sleep ; (n --)
             '(:dup 0 := :if 1 :- :sleep :else :drop :then))

(execute nil :bind 'end1?
         nil :bind 'end2?
         0 :bind 'val
         :noexec '(%sub val 1 :+ 100 :sleep :set! val t :set! end1?) nil :process-exec
         :noexec '(%sub val 1 :+ 100 :sleep :set! val t :set! end2?) nil :process-exec
         :noexec '(%sub end1?) :await
         :noexec '(%sub end2?) :await
         'val :print
         :unbind
         :unbind
         :unbind)


;; -------
;; 4. Jeden proces tiskne 1, 3 a druhý 2, 4.
;; -------

(execute nil :bind 'arrive1?
         nil :bind 'arrive2?
         :noexec '(%sub 1 :print 3 :print) nil :process-exec
         :noexec '(%sub 2 :print 4 :print) nil :process-exec
         :unbind
         :unbind)

;; Synchronizujte procesy tak, aby se nejdříve vytisklo 1, 2 a až poté 2, 4.
