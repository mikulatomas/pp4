(in-package "CL-USER")

;; Umožní v souborech používat diakritiku (není to standardní Common Lisp):
(set-default-character-element-type 'simple-char)

(defsystem kanren ()
  :members ("09_kanren_1" "09_kanren_2" "10_kanren_3")
  :rules ((:compile :all 
           (:requires (:load :previous)))))

(compile-system 'kanren :load t)
