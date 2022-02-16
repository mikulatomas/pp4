;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 01_stacks_3.lisp - Zásobníkové programování
;;;
;;; Jazyk pro myšlený zásobníkový stroj.
;;; fáze 3 - Interpret Scheme: podprogramy
;;;
;;; Předchozí soubory musí být načteny.

#|
Směřujeme k vytvoření interpretu Scheme pro náš zásobníkový stroj.

V tomto souboru naprogramujeme možnost zavést podprogramy
Budou to hodnoty (mohou se vyskytovat na datovém zásobníku),
které ale půjde spustit podobně jako slova. 
Spuštění podprogramu umístěného na datovém zásobníku se zařídí přesunutím
podprogramu zpět na programový zásobník. 

|#

;; Přehození vrcholu programového zásobníku na zásobník výsledků.
;; (obdoba lispového quote)
(define-word :noexec (lambda ()
                       (push (pop *exec*) *rslt*)))

;; Přesun z datového zásobníku na zásobník programový slovem :execute.
;; Přesouvá se až na druhé místo, kdybychom před spuštěním chtěli ještě 
;; něco udělat. Když nechceme dělat nic, můžeme použít slovo :nop
;; (No Operation)

(define-word :nop (lambda ()))

(define-word :execute (lambda ()
                        (let ((old-top (pop *exec*)))
                          (push (pop *rslt*) *exec*)
                          (push old-top *exec*))))

;; Většinou ale nechceme dělat nic, takže si na to uděláme další slovo:
(define-word :exec '(:execute :nop))

;; Podprogram je hodnota obsahující kód.
;; Reprezentujeme jej seznamem s prvním prvkem %sub.
;; Seznamy jako vykonatelné hodnoty na zásobníku exec jsme ještě
;; neměli. Vždy, když se na zásobníku objeví, bude to mít něco
;; společného s podprogramy a Schemem.

;; Když je podprogram na vrcholu programového zásobníku, vezme se
;; jeho kód a dá se na zásobník jeden prvek po druhém.
;; Podprogram je reprezentovaný seznamem (%sub . code),
;; kde code je jeho kód v seznamu.

;;
;; Spuštění podprogramu (byl na vrcholu programového zásobníku):
;; funkce exec-sub
;;


(defmethod exec-list ((op (eql '%sub)) args)
  (setf *exec* (append args *exec*)))


;;
;; Krok vyhodnocení (úprava funkce z předchozí fáze)
;;

(defmethod exec-elem ((elem list))
  (exec-list (car elem) (cdr elem)))


(defmethod exec-list (op args)
  (error "Unknown element on exec stack: ~s" (cons op args)))


;; (vysvětlení k proměnné dspec:*redefinition-action* viz v předchozím souboru)


;; Definice podprogramů na aritmetické operace:
(execute :noexec '(%sub :+) :bind '+)
(execute :noexec '(%sub :-) :bind '-) 
(execute :noexec '(%sub :*) :bind '*)
(execute :noexec '(%sub :/) :bind '/)
(execute :noexec '(%sub :=) :bind '=)

;; Definice podprogramu pro tisk:
(execute :noexec '(%sub :print) :bind 'print)

#|

;; Příklady 

(execute 2 1 '- :exec)

(execute 1 'print :exec)

(execute 5 '(%sub 2 * :exec))


;; faktoriál jako procedura: 

(execute :noexec '(%sub :dup 0 := :if :dup 1 :- fact :exec :* :else :drop 1 :then)
         :bind 'fact)

(execute 5 'fact :exec)

|#

#|
Podprogram může obsahovat libovolný kód, může si tedy dělat se zásobníky, co chce.
Můžeme jej ale přiblížit procedurám (funkcím) z vyšších programovacích jazyků
využitím parametrů. Například kód procedury s jedním parametrem by mohl vypadat takto:

:noexec x :bind ... kód využívající parametr x ... :unbind

Výsledek je o něco čitelnější. Například faktoriál:

(execute :noexec '(%sub :bind n
                           n 0 := :if n n 1 :- fact :exec :* :else
                                      1 :then
                        :unbind)
         :bind 'fact)

(execute 5 'fact :exec)

;; Více parametrů je třeba navazovat v obráceném pořadí.
;; např. výpočet druhé mocniny diskriminantu kvadratické rovnice s koeficienty
;; a, b, c:

(execute :noexec '(%sub :bind c 
                        :bind b 
                        :bind a 
                        b b :* 4 a c :* :* :-
                        :unbind
                        :unbind
                        :unbind)
         :bind 'sqdiscr)

(execute 6 11 -2 'sqdiscr :exec)

|#


