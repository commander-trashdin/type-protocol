


(in-package #:type-protocol)

(defun %dimensions-comp (dimensions)
  (cond ((eql '* dimensions) 0)
        ((listp dimensions) (mapcar (lambda (x) (if (eql '* x) 0 x)) dimensions))
        (t dimensions)))


(defmethod typename->protocol ((typename (eql 'hash-table)) &optional parameters)
  (assert (not parameters))
  (make-instance 'arbitarty-size-container-type
                 :name 'hash-table
                 :make-with #'make-hash-table
                 :short-name 'hash-table
                 :contains (mapcar #'typename->protocol '(t t))
                 :default `(make-hash-table)))



(defmethod typename->protocol ((typename (eql 'array)) &optional parameters)
  (when parameters
    (assert (and (listp parameters) (<= 2 (length parameters)))))
  (make-instance 'arbitarty-size-container-type
                 :name 'array
                 :make-with #'make-array
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
