(in-package #:type-protocol)




(defmethod typename->protocol ((typename (eql '*)) &optional parameters)
  (declare (ignorable parameters))
  typename)


(defmethod typename->protocol ((typename (eql 'eql)) &optional parameters)
  (assert (and (listp parameters) (= 1 (length parameters))))
  (let ((object (car parameters)))
    (unless (or (numberp object) (characterp object) (symbolp object))
      (warn "It looks like defined type doesn't use one of the proper non-immutable objects.
             While technically correct, such a type would not be considered type of anything
             except for the object used in its declaration itself."))
    (make-instance 'enum :name 'eql
                   :default-form object
                   :vals parameters)))

(defmethod typename->protocol ((typename (eql 'member)) &optional parameters)
  (assert (and (listp parameters) (< 0 (length parameters))))
  (unless (every (lambda (x) (or (numberp x) (characterp x) (symbolp x)))
                 parameters)
      (warn "It looks like defined type doesn't use one of the proper non-immutable objects.
             While technically correct, such a type would not include the similiar objects,
             except for the object used in its declaration itself."))
  (make-instance 'enum :name 'member
                 :default-form (car parameters)
                 :vals parameters))



(defmethod typename->protocol ((typename (eql 'or)) &optional parameters)
  (assert (and (listp parameters) (< 0 (length parameters))))
  (make-instance 'set-like :name 'or
                 :default-form (default (car parameters))
                 :types (mapcar #'typename->protocol parameters)))


(defmethod typename->protocol ((typename (eql 'and)) &optional parameters)
  (assert (and (listp parameters) (< 0 (length parameters))))
  (make-instance 'set-like :name 'and
                 :types (mapcar #'typename->protocol parameters)))


(defmethod typename->protocol ((typename (eql 'not)) &optional parameters)
  (assert (and (listp parameters) (= 1 (length parameters))))
  (make-instance 'set-like :name 'not
                 :types (typename->protocol (car parameters))))
