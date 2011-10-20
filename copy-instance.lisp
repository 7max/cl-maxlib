;;; -*- Mode:Lisp; Syntax:ANSI-Common-Lisp; Coding:utf-8 -*-
;; Generic (shallow) object copying for CLOS
;; Author: Michael Weber <michaelw@foldr.org>, 2008

;;; Notes:
;; This is a simple, but rather slow method of copying objects.  It
;; would be faster if COPY-INSTANCE methods would be partially
;; specialized on the class.  In other words: unroll the slot-copying
;; loop, and optoinally get rid of SLOT-BOUNDP checks if the users
;; says it is safe.  Care would need to be taken to regenerate this
;; specialized method if the result of COPY-CLASS-SLOTS changes,
;; either by class redefinition, or by adding methods on
;; COPY-CLASS-SLOTS.

;;;; Code
(cl:in-package :cl-maxlib)

(def (generic e) slot-list-for-copy (class instance copy-instance-reason)
  (:documentation "Returns the set of slots of OBJ of CLASS which are
considered for copying by COPY-INSTANCE.

Default implementation returns CLASS-SLOTS

COPY-INSTANCE-REASON is an extra parameter passed through from
COPY-INSTANCE and COPY-INSTANCE-SLOTS that can be specialized on to
customize copy instance functionality")
  (:method ((class standard-class) instance copy-reason)
    (declare (ignore instance copy-reason))
    (class-slots class)))

(def (generic e) make-uninitialized-instance (class copy-instance-reason)
  (:documentation "Allocates a fresh uninitialized instance of the
given class CLASS.

Default implementation calls ALLOCATE-INSTANCE.

COPY-INSTANCE-REASON is an extra parameter passed through from
COPY-INSTANCE and COPY-INSTANCE-SLOTS that can be specialized on to
customize copy instance functionality")
  (:method ((class class) copy-instance-reason)
    (declare (ignore copy-instance-reason))
    (allocate-instance class)))

(def (generic e) copy-slot-value (object slot-name slot-value copy-instance-reason)
  (:documentation "When COPY-INSTANCE copies slots, it calls this
function to produce a new slot value. Default implementation simply
returns SLOT-VALUE producing a shallow copy")
  (:method (object slot-name slot-value copy-instance-reason)
    (declare (ignore object slot-name copy-instance-reason))
    slot-value))

(def (function e) copy-instance-slots (class object copy &optional copy-instance-reason)
  "Copy slot values from OBJECT to COPY. The list of slots to copy is
obtained by calling SLOT-LIST-FOR-COPY generic function and each slot
is copied by calling COPY-SLOT-VALUE.

COPY-INSTANCE-REASON is an extra parameter passed through from
COPY-INSTANCE and COPY-INSTANCE-SLOTS that can be specialized on to
customize copy instance functionality)"
  (dolist (slot (slot-list-for-copy class object copy-instance-reason))
    (let ((slot-name (slot-definition-name slot)))
      (if (slot-boundp object slot-name)
          (setf (slot-value copy slot-name)
                (copy-slot-value object slot-name
                                 (slot-value object slot-name)
                                 copy-instance-reason))
          (slot-makunbound copy slot-name)))))

(def (generic e) copy-instance (object &rest initargs &key &allow-other-keys)
  (:documentation "Makes and returns a \(shallow) copy of OBJECT.

An uninitialized object of the same class as OBJECT is allocated by
calling MAKE-UNINITIALIZED-INSTANCE.  For all slots returned by
\(SLOT-LIST-FOR-COPY \(CLASS-OF OBJECT)), the returned object has
the same slot values and slot-unbound status as OBJECT.

REINITIALIZE-INSTANCE is called to update the copy with INITARGS.")
  (:method ((object standard-object) &rest initargs &key copy-instance-reason &allow-other-keys)
    (let* ((class (class-of object))
           (copy (make-uninitialized-instance class copy-instance-reason)))
      (copy-instance-slots class object copy copy-instance-reason)
      (apply #'reinitialize-instance copy initargs))))
