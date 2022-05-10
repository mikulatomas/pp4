(in-package "CL-USER")

;; Umožní v souborech používat diakritiku (není to standardní Common Lisp):
(set-default-character-element-type 'simple-char)

(defsystem prolog ()
  :members ("09_kanren_1" "09_kanren_2" "10_kanren_3" "11_prolog_1")
  :rules ((:compile :all 
           (:requires (:load :previous)))))

(compile-system 'prolog :load t)
