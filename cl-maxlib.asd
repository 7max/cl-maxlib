(asdf:defsystem :cl-maxlib
  :serial t
  :depends-on (:closer-mop :demacs :iterate :alexandria :stefil)
  :components 
  ((:file "package") 
   (:file "copy-instance")
   (:file "maxlib")
   (:file "defseries")
   (:file "tests")))
