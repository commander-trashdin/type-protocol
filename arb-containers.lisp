


(in-package #:type-protocol)

(defun %dimensions-comp (dimensions)
  (cond ((eql '* dimensions) 0)
        ((listp dimensions) (mapcar (lambda (x) (if (eql '* x) 0 x)) dimensions))
        (t dimensions)))


(defmethod typename->protocol ((typename (eql 'hash-table)) &optional parameters)
  (assert (not parameters))
  (make-instance 'arbitarty-size-container-type
                 :name 'hash-table
                 :specificp t
                 :short-name 'hash-table
                 :contains (mapcar #'typename->protocol '(t t))
                 :default `(make-hash-table)))

(defmethod typename->protocol ((typename (eql 'list)) &optional parameters)
  (assert (not parameters))
  (make-instance 'arbitarty-size-container-type
                 :name 'list
                 :specificp t
                 :short-name 'list
                 :contains t
                 :default nil))


(defmethod typename->protocol ((typename (eql 'sequence)) &optional parameters)
  (assert (not parameters))
  (make-instance 'arbitarty-size-container-type
                 :name 'sequence
                 :specificp nil
                 :short-name 'sequence
                 :contains t))


(defmethod typename->protocol ((typename (eql 'array)) &optional parameters)
  (when parameters
    (assert (and (listp parameters) (<= 2 (length parameters)))))
  (make-instance 'arbitarty-size-container-type
                 :name 'array
                 :contains (if (first parameters)
                               (typename->protocol (first parameters))
                               '*)
                 :size (or (second parameters) '*)
                 :default `(make-array ',(if (= 2 (length parameters))
                                             (%dimensions-comp (second parameters))
                                             0)
                                       :element-type ',(or (first parameters) t)
                                       :initial-element ,(if (first parameters)
                                                             (default (typename->protocol (first parameters)))
                                                             0)
                                       :adjustable t)))

(defmethod typename->protocol ((typename (eql 'simple-array)) &optional parameters)
  (when parameters
    (assert (and (listp parameters) (<= 2 (length parameters)))))
  (make-instance 'arbitarty-size-container-type
                 :name 'array
                 :contains (if (first parameters)
                               (typename->protocol (first parameters))
                               '*)
                 :size (or (second parameters) '*)
                 :default `(make-array ',(if (= 2 (length parameters))
                                             (%dimensions-comp (second parameters))
                                             0)
                                       :element-type ',(or (first parameters) t)
                                       :initial-element ,(if (first parameters)
                                                             (default (typename->protocol (first parameters)))
                                                             0))))

(defmethod typename->protocol ((typename (eql 'vector)) &optional parameters)
  (when parameters
    (assert (and (listp parameters) (<= 2 (length parameters)))))
  (make-instance 'arbitarty-size-container-type
                 :name 'array
                 :contains (if (first parameters)
                               (typename->protocol (first parameters))
                               '*)
                 :size (or (second parameters) '*)
                 :default `(make-array ',(if (= 2 (length parameters))
                                             (%dimensions-comp (second parameters))
                                             0)
                                       :element-type ',(or (first parameters) t)
                                       :initial-element ,(if (first parameters)
                                                             (default (typename->protocol (first parameters)))
                                                             0)
                                       :adjustable t
                                       :fill-pointer 0)))



(defmethod typename->protocol ((typename (eql 'simple-vector)) &optional parameters)
  (when parameters
    (assert (and (listp parameters) (<= 1 (length parameters)))))
  (make-instance 'arbitarty-size-container-type
                 :name 'array
                 :contains (typename->protocol t)
                 :size (or (first parameters) '*)
                 :default `(make-array ',(if (= 2 (length parameters))
                                             (%dimensions-comp (first parameters))
                                             0))))



(defmethod typename->protocol ((typename (eql 'bit-vector)) &optional parameters)
  (when parameters
    (assert (and (listp parameters) (<= 1 (length parameters)))))
  (make-instance 'arbitarty-size-container-type
                 :name 'array
                 :contains (typename->protocol 'bit)
                 :size (or (first parameters) '*)
                 :default `(make-array ',(if (= 2 (length parameters))
                                             (%dimensions-comp (first parameters))
                                             0)
                                       :element-type 'bit
                                       :initial-element 0
                                       :adjustable t)))



(defmethod typename->protocol ((typename (eql 'string)) &optional parameters)
  (when parameters
    (assert (and (listp parameters) (<= 1 (length parameters)))))
  (make-instance 'arbitarty-size-container-type
                 :name 'array
                 :contains (typename->protocol 'character)
                 :size (or (first parameters) '*)
                 :default `(make-array ',(if (= 1 (length parameters))
                                             (%dimensions-comp (first parameters))
                                             0)
                                       :element-type 'character
                                       :initial-element #\Nul
                                       :adjustable t)))

(defmethod typename->protocol ((typename (eql 'simple-string)) &optional parameters)
  (when parameters
    (assert (and (listp parameters) (<= 1 (length parameters)))))
  (make-instance 'arbitarty-size-container-type
                 :name 'array
                 :contains (typename->protocol 'character)
                 :size (or (first parameters) '*)
                 :default `(make-array ',(if (= 1 (length parameters))
                                             (%dimensions-comp (first parameters))
                                             0)
                                       :element-type 'character
                                       :initial-element #\Nul)))

(defmethod typename->protocol ((typename (eql 'base-string)) &optional parameters)
  (when parameters
    (assert (and (listp parameters) (<= 1 (length parameters)))))
  (make-instance 'arbitarty-size-container-type
                 :name 'array
                 :contains (typename->protocol 'base-char)
                 :size (or (first parameters) '*)
                 :default `(make-array ',(if (= 1 (length parameters))
                                             (%dimensions-comp (first parameters))
                                             0)
                                       :element-type 'base-char
                                       :initial-element #\Nul
                                       :adjustable t)))


(defmethod typename->protocol ((typename (eql 'simple-base-string)) &optional parameters)
  (when parameters
    (assert (and (listp parameters) (<= 1 (length parameters)))))
  (make-instance 'arbitarty-size-container-type
                 :name 'array
                 :contains (typename->protocol 'base-char)
                 :size (or (first parameters) '*)
                 :default `(make-array ',(if (= 1 (length parameters))
                                             (%dimensions-comp (first parameters))
                                             0)
                                       :element-type 'base-char
                                       :initial-element #\Nul
                                       :adjustable t)))
