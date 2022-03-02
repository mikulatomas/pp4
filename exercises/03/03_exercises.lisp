;; -------
;; 1. Přepište do paralelního scheme
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
;; 2. Mějme následující paralelní algoritmus

;; ---------------------------------------------
;;                    n = 0
;; ---------------------------------------------
;;          p           |           q
;; ---------------------------------------------
;;  while n < 2         |   n = n + 1
;;      print(n)        |   n = n + 1
;; ---------------------------------------------

;; a) Vytvořte historii, která výpíše posloupnosti 012, 002, 02.
;; b) Musí se hodnota 2 objevit ve výstupu vždy?
;; c) Kolikrát se může hodnota 2 objevit ve výstupu?
;; d) Kolikrát se může hodnota 1 objevit ve výstupu?
;; -------

;; -------
;; 3. Výše zmíněný algoritmus vytvořte v jazyce scheme
;; -------

(execute ...)
