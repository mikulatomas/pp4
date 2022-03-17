;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 02_stacks_5.lisp - Zásobníkové programování
;;;
;;; Jazyk pro myšlený zásobníkový stroj.
;;; fáze 5 - procesy
;;;
;;; Předchozí soubory musejí být načteny.

#|

Jak jsme si říkali, mezi jednotlivými kroky výpočtu na zásobníkovém stroji 
je stav výpočtu kompletně zaznamenán na zásobnících. Výpočet lze tedy 
kdykoli zastavit a zase spustit, pokud zásobníkům nastavíme jejich původní 
obsah. To děláme pomocí tzv. pokračování.

Pokračování je datová struktura, která uchovává obsah zásobníků:

- datový zásobník
- programový zásobník
- zásobník vazeb

Pokračování je reprezentováno seznamem
(%cont exec-stack rslt-stack bnd-stack)


Proces je reprezentován jako pokračování.
|#

;; Seznam procesů připravených k vykonávání (rprs je zkratka za Ready PRocesseS)
(defvar *rprs*)
(setf *rprs* '()) 


;; Uloží běžící proces do seznamu procesů připravených k běhu.
(defun save-running-process ()
  (push `(%cont ,*rslt* ,*exec* ,*bnd*) *rprs*))


;; Nastaví proces jako běžící.
(defun restore-process (process)
  (setf *rslt* (second process)
        *exec* (third process)
        *bnd* (fourth process)))


;; Čas do spuštění plánovače:
(defvar *time*)

;; Maximální čas nepřerušeného vykonávání procesu:
(defvar *max-time*)
(setf *max-time* 100)

;; Vybere náhodně k běhu jeden z připravených procesů nebo vrátí nil, pokud žádný proces není připravený. 
;; Vybraný proces odebere ze seznamu připravených procesů.
(defun take-ready-process ()
  (let ((process (nth (random (length *rprs*)) *rprs*)))
    (setf *rprs* (remove process *rprs*))
    process))


;; Plánovač
(defun scheduler ()
  (when *exec*
    (save-running-process))
  (when *rprs*
    (restore-process (take-ready-process)))
  (setf *time* (random *max-time*)))


;;
;; Spuštění programu.
;;

(setf dspec:*redefinition-action* :quiet)

(defun execute (&rest code)
  (let ((*rslt* '())
   (*exec* code)
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

;; Vykonání v procesu
;; Vytvoří proces, který bude mít na datovém zásobníku argument a na programovém podprogram 
(define-word :process-exec 
             (lambda ()
               (let ((arg (pop *rslt*))
                     (sub (pop *rslt*)))
                 (push `(%cont (,arg) (,sub) ,*bnd*)  
                       *rprs*))))




#|
; Většinou vrátí 1:
(execute :noexec '(%sub) 1 :process-exec 2)

;; Pouze na testování. Tiskne čísla od n po nulu.
(define-word :count-down ; (n --)
             '(:dup 0 := :if :print 1 :- :count-down :else :print :drop :then))


(execute 5 :count-down)

(execute :noexec '(%sub :print) "<-" :process-exec
         10 :count-down)

(execute :noexec '(%sub :print) "<-1" :process-exec :drop 
         :noexec '(%sub :print) "<-2" :process-exec :drop
         10 :count-down)
|#

;; Pravdivostní hodnoty
(execute '(define t 't))

(execute '(define nil 'nil))

;; Aktivní čekání. Opakovaně vykonává podprogram na vrcholu datového zásobníku.
;; Vykonání podprogramu musí způsobit efekt: (-- val) 
;; Opakování končí, když je hodnota val Pravda.
;;
;; (elem --)
(define-word :await '(:dup :exec :if :await :else :drop :then))


#|

; Hlavní proces bude čekat na skončení vedlejšího procesu:
(execute :noexec nil :bind 'end?
         :noexec '(%sub :print :noexec t :set! end?) 1 :process-exec 
         :noexec '(%sub end?) :await 
         :unbind)
|#




