(cl:in-package :cl-maxlib)

(def (macro e) eval-always (&body body)
  "Wraps BODY in (EVAL-WHEN (:COMPILE-TOPLEVEL :LOAD-TOPLEVEL :EXECUTE)) 

Should be used for helper functions that are called by macros, as
these need to be defined at compile time.

For example, the following code works fine when loading the file, but
fails when trying to compile the file (on some systems like SBCL):

 (defun foo-expander (&rest args)
   ... )

 (defmacro foo (&body body)
   (foo-expander body))

In order for macro to be able to call functions, or use any other definitions
such definitions need to be wrapped in (EVAL-ALWAYS). Below example will
work both when compiling, and when loading.

 (eval-always
   (defun foo-expander (&rest args)
   ... ))

 (defmacro foo (&body body)
   (foo-expander body))"
  `(eval-when (:compile-toplevel :load-toplevel :execute) 
     ,@body))

(def (macro e) defc (name-and-options &rest rest)
  "DEMACS definer that wraps its expansion in EVAL-ALWAYS. Should be
used for defining classes that are used with WITH-TYPED-SLOTS, and
helper functions that are called by macros."
  `(eval-always 
     (def ,name-and-options ,@rest)))

(defc (generic e) slot-typed-access-form (slot obj))

(defc method slot-typed-access-form ((slot effective-slot-definition) obj)
  "Returns the form used to access slots of CLOS classes"
  `(the ,(slot-definition-type slot) 
    (slot-value ,obj ',(slot-definition-name slot))))

#+sbcl
(defc method slot-typed-access-form ((slot sb-pcl::structure-effective-slot-definition) obj)
  "Returns the form used to access slots of SBCL defstruct"
  `(the ,(slot-definition-type slot) 
    (,(sb-pcl::slot-definition-defstruct-accessor-symbol slot) ,obj)))

(defc function define-class-typed-slots (class object form)
  "Returns a macrolet form that defines a symbol for each slot of a
class/struct to be the typed access form, ie (the <slot
type> (<slot-accessor> <slot-name>))"
  `(symbol-macrolet 
       (,@(loop for slot in (class-slots class)
	     collect (list (slot-definition-name slot)
			   (slot-typed-access-form slot object))))
     ,@form))

(def (macro e) with-typed-slots ((object class) &environment env &body body)
  "Defines a macrolet form for each each slot of the `object' of type
`class' that corresponds to the expression (the
<slot-type> (slot-accessor `object' <slot>))"
  (let ((obj (gensym)))
    `(let ((,obj ,object))
       (declare (type ,class ,obj))
       ,(define-class-typed-slots (ensure-finalized (find-class class env)) obj body))))

(defc (macro e) getcache (key hash &body default) 
  "Simplifies the very commonly used pattern of caching something in a hash table.

If KEY is not found in HASH then evaluate DEFAULT, add its value to
the hash and return it. If KEY was found in the HASH returns cached
value without evaluating DEFAULT.

Example usage pattern:

 (defvar *cache* (make-hash-table :test equal))

 (defun cached-calculation (&rest params)
   (getcache params *cache* (expansive-calculation params))) "
  (with-gensyms (k h) 
    `(let ((,k ,key)
          (,h ,hash))
      (or (gethash ,k ,h)
           (setf (gethash ,k ,h) 
                 (progn
                   ,@default))))))

#+sbcl
(defvar *mixins* (make-hash-table :test #'equal :synchronized t)
  "Hash table to cache the mixins we created")

#-sbcl
(defvar *mixins* (make-hash-table :test #'equal)
  "Hash table to cache the mixins we created")

(def (function e) make-mixin-class (&rest parents)
  "Create an anonymous class that has PARENTS as its base classes. The
metaclass of a new class will be the metaclass of the first class in
PARENTS. The class thusly created is cached, and if this function is
called again with the same parameters, the cached class willbe
returned:

Example:

 ;; class that collects some stats
 (defclass stats-collector ()(stats))

 ;; some operation after which we have to collect stats
 (defmethod some-op :after ((obj stats-collector))
   (with-slots (stats) obj
     (do-something-with stats)))

 ;; Add stats-collector as a parent class to an arbitrary object
 (defun add-stats-to-object (obj)
   (change-class obj (make-mixin-class (class-ob obj) 'stats-collector)))
"
  (flet ((%ensure-class (c)
           (if (typep c 'class) c
               (find-class c))))
    (let ((superclasses (mapcar #'%ensure-class parents)))
      (getcache superclasses *mixins*
                (make-instance (class-of (first superclasses)) 
                               :direct-superclasses superclasses)))))

(defmacro-clause (VCOLLECT expr &optional INTO var)
  "Like COLLECT but returns elements in the vector"
  `(accumulate ,expr
               by (lambda (x y)
                    (vector-push-extend x y)
                    y) 
               initial-value (make-array 
                              0 :adjustable t :fill-pointer t)
               into ,var))

(def (macro e) loop-while (pred &body body)
  "Execute body while PRED is true"
  (with-gensyms (start end) 
   `(tagbody 
       ,start
       (unless ,pred
         (go ,end))
       ,@body
       (go ,start)
       ,end)))


(def (function e) cat (&rest strings) 
    "A shortcut for (concatenate 'string foo bar)."
  (apply 'concatenate 'string strings))


(def (macro e) maximize-and-minimize (expr1 expr2)
  "Expands into the ITERATE clause that maximizes EXPR1 and minimizes
EXPR2 and returns results as two values"
  (with-unique-names (v1 v2)
    `(progn
       (maximizing ,expr1 into ,v1)
       (minimizing ,expr2 into ,v2)
       (finally (return (values ,v1 ,v2))))))





