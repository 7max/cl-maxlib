(asdf:defsystem :cl-maxlib
  :serial t
  :depends-on (:closer-mop :demacs :cl-log :iterate :alexandria :stefil)
  :components 
  ((:file "package") 
   (:file "maxlib")
   (:file "defseries")
   (:file "tests"))
  :depends-on (:closer-mop :demacs :cl-log :iterate :alexandria :stefil))
