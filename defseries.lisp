
(cl:in-package :cl-maxlib)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (import '(demacs::struct-options-of 
            demacs::name-of
            demacs::slot-descs-of
            demacs::documentation-of)))

(def (struct ea) (series (:constructor nil))
  (length 0 :type fixnum)
  (capacity 0 :type fixnum))

(def (print-object :print-identity t :print-type t) series (self)
  (format t "len=~d capacity=~d" (series-length self) (series-capacity self)))

(def (generic e) make-series (name)) 
(def (generic e) series-shallow-copy (series)) 
(def (generic e) series-deep-copy (series)) 
(def (generic e) series-ensure-capacity (series size))
(def (generic e) series-allocate-capacity (series size)) 

(def (function e) series-trim-capacity (series)
  "Trim series capacity to series length"
  (series-allocate-capacity series (series-length series)))

;; all the helper structures and function have to be defined
;; doing compile time also, so wrap everything into eval-when

(defc (struct) (series-desc (:type list))
  (columns nil)
  (increase-capacity nil)
  (check-capacity nil)
  (series-add nil)
  (series-add-from nil)
  (series-add-values nil)
  (with-series nil)
  (series-values nil)
  (series-values-as-list nil)
  (make-series-name nil)
  (shallow-copy-series nil)
  (deep-copy-series nil)
  (series-ensure-capacity nil)
  (series-allocate-capacity nil))

(def (struct) (series-column (:type list)) 
  (name nil)
  (series nil)
  (type nil)
  (default nil)
  ;; below are various symbols
  (column-type nil)
  (array-type nil)
  (array-accessor nil))

(defc special-variable *series* (make-hash-table))

(defc function format-series-symbol (format &rest args)
  (intern (string-upcase (apply #'format nil format args))))

(defc struct (col-binding (:type list)) name var type accessor)
(defc struct (col-desc (:type list)) column as at binding type)

(defc function maybe-add-col-binding (list column)
  "Adds the col-binding to the list if its not already there. Needed because
both symbols-macrolets and macrolets in with-series-expander share teh same 
binding for the series column arrays. Returns the new list"
  (let ((found (find (series-column-name column) list :key #'col-binding-name)))
    (unless found 
      (push
       (setq found (make-col-binding 
                    :name (series-column-name column)
                    :var (gensym)
                    :type (series-column-array-type column)
                    :accessor (series-column-array-accessor column)))
       list))
    (values list found)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun get-col-binding (binds col)
    "Find a col-binding for the column in the list of binds. Throws
  error if not found"
    (or (find (col-desc-column col) binds 
              :key #'col-binding-name)
        (error "Unable to find column ~s in binding list ~s" 
               (col-desc-column col) binds))))

(defc function series-values-expander (series-name series-expr global-at)
  "Expands into the definition of (series-name-values series-expr)
macro"
  (let* ((desc (gethash series-name *series*)) 
         (columns (series-desc-columns desc)))
    (with-gensyms (series index)
      `(let* ((,series ,series-expr)
              (,index ,(if (member global-at '(nil :default)) 
                           `(1- (series-length ,series))
                           global-at)))
         (declare (type ,series-name ,series) 
                  (type fixnum ,index))
         (values ,@(iter
                    (for col in columns)
                    (collect
                        `(the ,(series-column-column-type col)
                           (aref (the ,(series-column-array-type col) 
                                   (,(series-column-array-accessor col) 
                                     ,series)) ,index)))))))))

(defc function with-series-expander (series-name series-expr global-at columns
                                                 accessors
                                                 var for iter from below to 
                                                 body)
  "Expands into the definition of (with-series-name series-expr
columns) macro"
  (with-gensyms (index indexes tmp)
    (let* ((series (or var (gensym "SERIES")))
           default-at-expr
           (desc (gethash series-name *series*))
           (all-columns (series-desc-columns desc))
           iteration-p
           (cl-package (find-package :cl))
           limit-expr
           binds bind
           (columns
            (iter (for col in columns)
              (destructuring-bind (col &key as at)
                  (ensure-list col)
                (let ((column (find col all-columns :key #'series-column-name)))
                  (or column (error "Column ~S not found" col))
                  (multiple-value-setq (binds bind)
                    (maybe-add-col-binding binds column))
                  (collect (make-col-desc
                            :column col
                            :as (or as col)
                            :at (or at global-at)
                            :binding bind
                            :type (series-column-column-type column)))))))
           (accessors
            (iter (for col in accessors)
              (destructuring-bind (col &key as)
                  (ensure-list col)
                (let ((column (find col all-columns :key #'series-column-name)))
                  (or column (error "Column ~S not found" col))
                  (multiple-value-setq (binds bind)
                    (maybe-add-col-binding binds column))
                  ;; do not add column accessor with the name
                  ;; of symbol in CL package, as SBCL gives
                  ;; package lock error when one tries to macrolet
                  ;; OPEN or such
                  (unless (and (null as) 
                               (eq (symbol-package col) cl-package))
                    (collect (make-col-desc
                              :column col
                              :as (or as col)
                              :binding bind
                              :type (series-column-column-type column)))))))))
      (cond ((or for from iter) 
             (and for iter (error "Only one of :iter or :for can be specified"))
             (setq iteration-p t)
             (setq for (or for iter (gensym "I")))
             (setq default-at-expr for)
             (setq from (or from 0))
             (cond ((and below to)
                    (error "Only one of :below or :to can be specified"))
                   (to 
                    (setq limit-expr (if iter `(to (the fixnum ,to))
                                         `(to ,to))))
                   (below
                    (setq limit-expr (if iter `(below (the fixnum ,below))
                                         `(below ,below))))
                   (t
                    (setq limit-expr (if iter `(below (the fixnum (series-length ,series)))
                                         `(below (series-length ,series)))))))
            (t
             (setq default-at-expr `(1- (series-length ,series)))
             (when (or to below)
               (error ":to/:below cannot be used without :for/:from"))))
      `(let ((,series ,series-expr))
         (declare (type ,series-name ,series))
         (let (,@(iter (for bind in binds)
                       (collect `(,(col-binding-var bind)
                                   (,(col-binding-accessor bind) ,series)))))
           (declare 
            ,@(iter (for bind in binds)
                    (collect `(type ,(col-binding-type bind)
                                    ,(col-binding-var bind)))
                    (collect `(ignorable ,(col-binding-var bind)))))
           (symbol-macrolet 
               (,@(iter (for col in columns)
                        (for bind = (get-col-binding binds col))
                        (collect 
                            `(,(col-desc-as col)
                               (the ,(col-desc-type col)
                                 (aref ,(col-binding-var bind)
                                       ,(if (member (col-desc-at col)
                                                    '(nil :default))
                                            default-at-expr 
                                            (col-desc-at col))))))))
             (macrolet 
                 (,@(iter (for col in accessors)
                          (for bind = (get-col-binding binds col))
                          (collect 
                              `(,(col-desc-as col) (,index &rest ,indexes)
                                 (if (null ,indexes)
                                     `(the ,',(col-desc-type col)
                                        (aref ,',(col-binding-var bind)
                                              ,,index))
                                     `(values 
                                       ,@(iter 
                                          (for ,tmp in (cons ,index ,indexes))
                                          (collect 
                                              `(the ,',(col-desc-type col)
                                                 (aref ,',(col-binding-var bind)
                                                       ,,tmp))))))))))
               ,@(if iteration-p 
                     (if iter
                         `((iterate 
                             (declare (iterate:declare-variables))
                             (for (the fixnum ,for) from ,from ,@limit-expr) 
                             ,@body))
                         `((loop for ,for fixnum from ,from ,@limit-expr 
                              do (progn
                                   ,@body))))
                     body))))))))

(defc function series-add-from-expander (series-name series1-expr series2-expr at)
  "Expands the (series-name-add-from) macro"
  `(multiple-value-call
       (function ,(series-desc-series-add-values (gethash series-name *series*)))
     ,series1-expr
     ,(series-values-expander series-name series2-expr at)))

(defc function series-add-expander (series-name series-expr &rest vals)
  "Expands into the definition of (series-name-add series-expr &key columns) macro"
  (with-gensyms (series index)
    (let ((desc (gethash series-name *series*)))
      `(let* ((,series ,series-expr)
              (,index (series-length ,series)))
         (declare (type ,series-name ,series)
                  (type fixnum ,index))
         (,(series-desc-check-capacity desc)
           ,series (incf (series-length ,series)))
         ,@(iter (for c in (series-desc-columns desc))
                 (for v in vals)
                 (collect 
                     `(setf (the ,(series-column-column-type c)
                              (aref 
                               (the ,(series-column-array-type c)
                                 (,(series-column-array-accessor c) ,series))
                               ,index))
                            ,v)))
         ,series))))

(defc function %defseries (series-name-and-options column-specs)
  (let* ((have-options-p (listp series-name-and-options))
         (series-name (if have-options-p (first series-name-and-options)
                          series-name-and-options))
         (inherit-from (or (and have-options-p
                                (second (assoc :include (rest series-name-and-options)))) 
                           'series))
         (parent-columns (unless (eq inherit-from 'series)
                           (series-desc-columns 
                            (gethash inherit-from *series*))))
         (all-column-names)
         (desc (make-series-desc
                :increase-capacity (format-series-symbol "increase-capacity-~s" series-name)
                :check-capacity (format-series-symbol "check-capacity-~s" series-name)
                :series-add (format-series-symbol "~s-add" series-name)
                :series-add-from (format-series-symbol "~s-add-from" series-name)
                :series-add-values (format-series-symbol "~s-add-values" series-name)
                :with-series (format-series-symbol "with-~s" series-name)
                :series-values (format-series-symbol "~s-values" series-name)
                :series-values-as-list (format-series-symbol "~s-values-as-list" series-name)
                :make-series-name (format-series-symbol "make-~s" series-name)
                :shallow-copy-series (format-series-symbol "~s-shallow-copy" series-name)
                :deep-copy-series (format-series-symbol "~s-deep-copy" series-name)
                :series-ensure-capacity (format-series-symbol "~s-ensure-capacity" series-name)
                :series-allocate-capacity (format-series-symbol "~s-allocate-capacity" series-name))))
    (iterate 
      (for column in column-specs)
      ;; types
      (let* ((column-name (first column))
             (type (getf (cddr column) :type t))
             (column (make-series-column 
                      :name column-name 
                      :series series-name
                      :default (second column)
                      :type type
                      :column-type (format-series-symbol "~s-~s-column-type" 
                                                         series-name 
                                                         column-name)
                      :array-type (format-series-symbol "~s-~s-type" 
                                                        series-name 
                                                        column-name)
                      :array-accessor (format-series-symbol "~s-~s" 
                                                            series-name 
                                                            column-name))))
        (when (find column-name parent-columns :key #'series-column-name)
          (error "Duplicate column name ~S" column-name))
        (collect `(deftype ,(series-column-column-type column) () ',type)
          into types1)
        (collect `(deftype ,(series-column-array-type column) () 
                    '(simple-array ,(series-column-column-type column) *))
          into types2)
        (collect `(,column-name nil :type (or ,(series-column-array-type column) null))
          into defs)
        (collect column into columns))
      (finally 
       (setf columns (append parent-columns columns))
       (setf (series-desc-columns desc) columns)
       (setf (gethash series-name *series*) desc)
       (setf all-column-names (mapcar #'series-column-name columns))
       (return
         `(progn 
            (eval-when (:load-toplevel :compile-toplevel :execute)
              (setf (gethash ',series-name *series*) ',desc))

            ,@types1 
            ,@types2

            (defstruct (,series-name 
                         (:include ,inherit-from))
              ,@defs)

            (defmethod make-series ((name (eql ',series-name)))
              (,(series-desc-make-series-name desc)))

            (defmethod series-shallow-copy ((series ,series-name))
              (,(series-desc-shallow-copy-series desc) series))

            (defmethod series-deep-copy ((series ,series-name))
              (,(series-desc-deep-copy-series desc) series))

            (defmethod series-ensure-capacity ((series ,series-name) size)
              (,(series-desc-series-ensure-capacity desc) series size))
            
            (defmethod series-allocate-capacity ((series ,series-name) size)
              (,(series-desc-series-allocate-capacity desc) series size))

            (defun ,(series-desc-series-ensure-capacity desc) (series size)
              (declare (type ,series-name series) 
                       (type fixnum size))
              (,(series-desc-increase-capacity desc) series size))

            (defun ,(series-desc-series-allocate-capacity desc) (series size)
              (declare (type ,series-name series) 
                       (type fixnum size))
              (when (< size (series-length series))
                (error "Can not allocate series capacity below its length"))
              (setf (series-capacity series) size)
              (,(series-desc-increase-capacity desc) series size))

            (defun ,(series-desc-shallow-copy-series desc) (series)
              (declare (type ,series-name series))
              (,(series-desc-make-series-name desc) 
                :length (series-length series) 
                :capacity (series-length series) 
                ,@(iter (for column in columns)
                        (collect (make-keyword (series-column-name column)))
                        (collect `(,(series-column-array-accessor column) series)))))
            
            (defun ,(series-desc-deep-copy-series desc) (series)
              (declare (type ,series-name series))
              (,(series-desc-make-series-name desc) 
                :length (series-length series) 
                :capacity (series-capacity series)
                ,@(iter (for column in columns)
                        (collect (make-keyword (series-column-name column)))
                        (collect `(make-array 
                                   (length (the ,(series-column-array-type column) 
                                             (,(series-column-array-accessor column) series))) 
                                   :element-type ',(series-column-column-type column)
                                   :initial-contents (the ,(series-column-array-type column) 
                                                       (,(series-column-array-accessor column) series)))))))

            (defmacro ,(series-desc-series-values desc) 
                (series &key (at :default))
              "Return all columns of a series at the specified index
as multiple-values."
              (series-values-expander ',series-name series at))

            (defmacro ,(series-desc-series-values-as-list desc) 
                (series &key (at :default)) 
              "Return all columns of a series at the specified index
as list"
              `(multiple-value-list 
                ,(series-values-expander ',series-name series at)))

            (defmacro ,(series-desc-with-series desc)
                ((series &key 
                         (at :default) 
                         (columns :all) 
                         (accessors :all)
                         var
                         for 
                         iter
                         from
                         below
                         to)
                 &body body)
              "Evaluate forms in the BODY with series columns bound to
symbols and/or accessor functions.

AT is an index at which series is accessed. A special value of :default
means the last record in the series.

VAR is a variable to which SERIES is bound. If not specified then it will
be gensym'ed. 


Iteration:

If :FOR VAR or :ITER VAR or :FROM INDEX keyword is specified, then
BODY will be iterated over all elements of the series, with index
variable bound to :FOR or :ITER variable. If :FROM is specified
but :FOR/:ITER is not, index variable will be gensymed. If :FOR was
used then iteration is done using (LOOP) and if :ITER was specified
then iteration is done using (ITERATE).

When doing iteration the :default value of AT keyword will be the
iteration index variable instead of last element of the series

Columns/Accessors:

COLUMNS is a list of columns to bind to symbols. Each list element is
either a symbol of one of the columns in teh series, or it can also
have a form of (COL [:as SYMBOL] [:at INDEX]), to use different
binding symbol from the column name, or bind it at different position
then the entire series AT parameter.

A special value of :ALL for the COLUMNS (which is a default) causes
all columns to be bound under their own column names.

ACCESORS is a similar list of columns to bind, except these are bound
to macrolets that accept index parameter.. Eath element of ACCESSORS
can be a symbol name, or a (COL [:as symbol])

The corresponding macrolets for each column will have the form
of: (COL index &rest indexes). If only one index is specified, then
macrolets will expand into SETF'able AREF form. If multiple indexes
are specified, they will expand into (VALUES) form returning elements
at each specified index.
"
              (with-series-expander 
                  ',series-name 
                series at 
                (if (eq columns :all) ',all-column-names columns)
                (if (eq accessors :all) ',all-column-names accessors)
                var
                for
                iter
                from
                below
                to
                body))

            (defmacro ,(series-desc-series-add desc)
                (series &key ,@(mapcar (lambda (column)
                                         (if (series-column-default column) 
                                             `(,(series-column-name column)
                                                ',(series-column-default column))
                                             (series-column-name column))) 
                                       columns))
              (series-add-expander ',series-name series ,@all-column-names))
            
            (defmacro ,(series-desc-series-add-from desc) 
                (series1 series2 &key (at :default))
              "Add the record at specified index from SERIES1 to the end of SERIES1"
              (series-add-from-expander ',series-name series1 series2 at))
            
            (defun ,(series-desc-series-add-values desc)
                (series ,@all-column-names)
              "Add the specified values at the end of the SERIES" 
              ,(apply #'series-add-expander series-name 'series all-column-names))

            (proclaim '(inline ,(series-desc-check-capacity desc)))

            (defun ,(series-desc-check-capacity desc)
                (series size)
              "Check if series capacacity is above the specified size,
and increase it if nessesary"
              (declare (type series series) (type fixnum size))
              (when (> size (series-capacity series))
                (,(series-desc-increase-capacity desc) series size))
              (values))

            (defun ,(series-desc-increase-capacity desc)
                (series size)
              "Increase the series capacity to at least specified size" 
              (declare (type series series)
                       (type fixnum size))
              (iter 
                (while (< (series-capacity series) size))
                (setf (series-capacity series)
                      (cond ((zerop (series-capacity series))
                             1)
                            ((< (series-capacity series) 512)
                             (the fixnum (* 8 (series-capacity series))))
                            (t 
                             (the fixnum (* 2 (series-capacity series)))))))
              ,@(iter (for column in columns)
                      (let ((accessor 
                             (series-column-array-accessor column))
                            (array-type (series-column-array-type column)))
                        (collect 
                            `(setf 
                              (,accessor series)
                              (let ((array (make-array 
                                            (series-capacity series) 
                                            :element-type ',(series-column-column-type column)))
                                    (old (,accessor series)))
                                (declare (type ,array-type array)
                                         (type (or ,array-type null array)))
                                (when old
                                  (replace array (the ,array-type old)))
                                array)))))
              (values))))))))


(defc (class nce) series-definer (struct-definer)
  ())

(def method expand-definer ((definer series-definer))
  (let* ((form (%defseries `(,(name-of definer) ,@(struct-options-of definer))
                           (slot-descs-of definer)))
         (series (gethash (name-of definer) *series*)))
    `(progn 
       ,form
       ,@(when (has-option-p definer #\e) 
               (export-now-and-later 
                (cons (name-of definer) 
                      (rest series)))) ;; everything after columns are symbols
       ,@(when (has-option-p definer #\s) 
               (export-now-and-later 
                (mapcar #'series-column-name (series-desc-columns series))))
       ,@(when (has-option-p definer #\a)
               (export-now-and-later 
                (let ((series (gethash (name-of definer) *series*)))
                  (mapcar #'series-column-array-accessor 
                          (series-desc-columns series))))))))

(def (macro e) defseries (series-name-and-options &rest column-specs)
  (%defseries series-name-and-options column-specs))



