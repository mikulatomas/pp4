;; -------
;; 1. Logický operátor or
;; -------

(define-word :or
             (lambda () 
               (let ((arg1 (pop *rslt*)))
                 (push (or (pop *rslt*) arg) *rslt*))))

(execute :noexec '(%sub :or) :bind 'or)