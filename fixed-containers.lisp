


(in-package #:type-protocol)


(defmethod typename->protocol ((typename (eql 'cons)) &optional parameters)
  (if parameters
      (progn
        (assert (and (listp parameters) (<= 2 (length parameters))))
        (make-instance 'fixed-size-container-type
                       :name typename
                       :contains (if (= 2 (length parameters))
                                     (mapcar #'typename->protocol parameters)
                                     (list (typename->protocol (car parameters)) '*))
                       :specificp t
                       :size 2))
      (make-instance 'fixed-size-container-type
                     :name typename
                     :contains '(* *)
                     :specificp t
                     :size 2)))
