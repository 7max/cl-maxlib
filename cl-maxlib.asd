(asdf:defsystem :cl-maxlib
  :serial t
  :depends-on (:closer-mop :demacs :iterate :alexandria :stefil)
  :components 
  ((:file "package") 
   (:file "maxlib")
   (:file "defseries")
   (:file "tests")))
