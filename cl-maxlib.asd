(asdf:defsystem :cl-maxlib
  :serial t
  :components 
  ((:file "package") 
   (:file "maxlib")
   (:file "defseries")
   (:file "tests"))
  :depends-on (:closer-mop :demacs :cl-log :iterate :alexandria :stefil))
