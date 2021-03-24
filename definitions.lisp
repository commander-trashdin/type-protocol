

(in-package #:type-protocol)


(defclass anytype ()
  ((name
    :type (or list symbol)
    :reader name
    :initarg :name)))


(defun expand (type-specifier &optional env)
  "Expands the TYPE-SPECIFIER to a canonical representation in the ENV."
  ;; TODO Fix for other then SBCL to accept the environment.
  ;; Since this isn't my code, I'm unsure what exactly "fix" was supposed to mean
  (declare (ignorable env))
  #+abcl          (system::expand-deftype type-specifier)
  #+xcl           (system::expand-deftype type-specifier)
  #+allegro       (excl:normalize-type type :default type-specifier)
  #+ccl           (ccl::type-expand type-specifier)
  #+clisp         (ext:type-expand type-specifier)
  #+cmu           (kernel:type-expand type-specifier)
  #+ecl           (si::expand-deftype type-specifier)
  #+mkcl          (si::expand-deftype type-specifier)
  #+lispworks     (type:expand-user-type type-specifier)
  #+sbcl          (sb-ext:typexpand type-specifier env)
  #-(or abcl allegro ccl clisp cmu ecl lispworks mkcl sbcl xcl)
  (assert nil nil "EXPAND unimplemented."))

;;; Abstractions

(defclass value-type (anytype)
  ((short-name
    :type symbol
    :reader short-name
    :initarg :short)
   (user-defined-p
    :type boolean
    :initform nil
    :reader user-defined-p)
   (default-form
     :reader default
     :initarg :default)
   (constructor
    :reader constructor
    :initarg :make-with
    :initform nil)))

(defclass container-type (value-type)
  ())


;;; Actual types


(defclass arbitarty-size-container-type (container-type)
  ((contained-type
    :type anytype
    :reader contained-type
    :initarg :contains)
   (size
    :type (or (eql *) integer list)
    :reader size
    :initarg :size)))


(defclass fixed-size-container-type (container-type)
  ((contained-types
    :type list
    :reader contained-types
    :initarg :contains)
   (size
    :type integer
    :reader size
    :initarg :size)))

(defmethod initialize-instance :after ((type fixed-size-container-type) &key)
  (with-slots (name short-name default-form contained-types size constructor) type
    (unless (slot-boundp type 'default-form)
      (when (and constructor (notany (lambda (x) (eql x '*)) contained-types))
        (setf default-form
              `(funcall ,constructor ,@(mapcar (lambda (x) `(default ,x)) contained-types))))))) ;;TODO Let's just imagine that this works for now




(defclass range-based-type (value-type)
  ((from
    :reader from
    :initarg :from)
   (to
    :reader to
    :initarg :to)))

(defmethod initialize-instance :after ((type range-based-type) &key)
  (with-slots (name short-name default-form from to) type
    (unless (slot-boundp type 'default-form)
      (cond ((eql short-name 'fixnum)
             (setf default-form 0))
            ((not (eql '* from))
             (setf default-form from))
            ((subtypep name 'number)
             (setf default-form 0))))
    (when (and (slot-boundp type 'short-name) (equalp name short-name))
      (if (and (eql '* from) (eql '* to))
          (setf short-name name)
          (case name                               ;;TODO cleanup?
            (integer
             (cond ((eql from 0)
                    (cond ((eql to 1)
                           (setf short-name 'bit))))
                   ((eql from most-negative-fixnum)
                    (cond ((eql to most-positive-fixnum)
                           (setf short-name 'fixnum))))))
            (float
             (cond ((and (eql from most-negative-short-float)
                       (eql to most-positive-short-float))
                    (setf short-name 'short-float))
                   ((and (eql from most-negative-single-float)
                       (eql to most-positive-single-float))
                    (setf short-name 'single-float))
                   ((and (eql from most-negative-double-float)
                       (eql to most-positive-double-float))
                    (setf short-name 'double-float))
                   ((and (eql from most-negative-long-float)
                       (eql to most-positive-long-float))
                    (setf short-name 'long-float)))))))))





;; Methods

(defgeneric typename->protocol (typename &optional parameters))
(defgeneric protocol->typename (protocol))



(defmethod typename->protocol ((typename (eql 't)) &optional parameters)
  (declare (ignorable parameters))
  (make-instance 'anytype
                 :name t))


(defmethod typename->protocol ((typename list) &optional parameters)
  (declare (ignorable parameters))
  (typename->protocol (first typename) (rest typename)))
