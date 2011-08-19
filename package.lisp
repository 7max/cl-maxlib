
(cl:defpackage :cl-maxlib
   (:use :closer-common-lisp :closer-mop
         :demacs
         :cl-log
         :iterate
         :alexandria
         :stefil)
   #.(cons :export (let (list)
                     (when (cl:find-package :cl-maxlib)
                       (cl:do-external-symbols (s :cl-maxlib list)
                         (cl:push s list)))))
   (:export :vcollect)
   (:shadowing-import-from :alexandria :when-let :ensure-list :ensure-function))
