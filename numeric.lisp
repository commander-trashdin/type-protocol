;;Methods for numeric types



(in-package :type-protocol)



(defmacro gen-number-range-method (name)
  `(defmethod typename->protocol ((typename (eql ,name)) &optional parameters)
     (if parameters
         (progn
           (assert (and (listp parameters) (<= (length parameters) 2)))
           (make-instance 'range-based-type
                          :name ,name
                          :short ,name
                          :from (etypecase (first parameters)
                                  (null '*)
                                  (number (first parameters))
                                  ((cons number null) (+ 1 (first parameters))))
                          :to (etypecase (second parameters)
                                (null '*)
                                (number (second parameters))
                                ((cons number null) (- (second parameters) 1)))))
         (make-instance 'range-based-type
                        :name ,name
                        :short ,name
                        :from '*
                        :to '*))))


(gen-number-range-method 'integer)
(gen-number-range-method 'float)
(gen-number-range-method 'real)
(gen-number-range-method 'rational)


(defmethod typename->protocol ((typename (eql 'fixnum)) &optional parameters)
  (assert (not parameters))
  (make-instance 'range-based-type
                 :name 'integer
                 :short typename
                 :from most-negative-fixnum
                 :to most-positive-fixnum))

(defmethod typename->protocol ((typename (eql 'bit)) &optional parameters)
  (assert (not parameters))
  (make-instance 'range-based-type
                 :name 'integer
                 :short typename
                 :from 0
                 :to 1))

(defmethod typename->protocol ((typename (eql 'short-float)) &optional parameters)
  (assert (not parameters))
  (make-instance 'range-based-type
                 :name 'float
                 :short typename
                 :from most-negative-short-float
                 :to most-positive-short-float))

(defmethod typename->protocol ((typename (eql 'single-float)) &optional parameters)
  (assert (not parameters))
  (make-instance 'range-based-type
                 :name 'float
                 :short typename
                 :from most-negative-single-float
                 :to most-positive-single-float))

(defmethod typename->protocol ((typename (eql 'double-float)) &optional parameters)
  (assert (not parameters))
  (make-instance 'range-based-type
                 :name 'float
                 :short typename
                 :from most-negative-double-float
                 :to most-positive-double-float))

(defmethod typename->protocol ((typename (eql 'long-float)) &optional parameters)
  (assert (not parameters))
  (make-instance 'range-based-type
                 :name 'float
                 :short typename
                 :from most-negative-long-float
                 :to most-positive-long-float))

(defmethod typename->protocol ((typename (eql 'signed-byte)) &optional parameters)
  (assert (and (listp parameters) (= 1 (length parameters)) (typep (car parameters) '(integer 0))))
  (let ((pow (car parameters)))
    (make-instance 'range-based-type
                   :name 'integer
                   :from (- (expt 2 pow))
                   :to (1- (expt 2 pow)))))


(defmethod typename->protocol ((typename (eql 'unsigned-byte)) &optional parameters)
  (assert (and (listp parameters) (= 1 (length parameters)) (typep (car parameters) '(integer 0))))
  (let ((pow (car parameters)))
    (make-instance 'range-based-type
                   :name 'integer
                   :from 0
                   :to (1- (expt 2 pow)))))


(defmethod typename->protocol ((typename (eql 'mod)) &optional parameters)
  (assert (and (listp parameters) (= 1 (length parameters)) (typep (car parameters) '(integer 0))))
  (let ((lim (car parameters)))
    (make-instance 'range-based-type
                   :name 'integer
                   :from 0
                   :to (1- lim))))


(defmethod typename->protocol ((typename (eql 'complex)) &optional parameters)
  (if parameters
      (progn
        (assert (and (listp parameters) (= 1 (length parameters)) (subtypep (car parameters) 'real)))
        (make-instance 'fixed-size-container-type
                       :name typename
                       :contains (list (typename->protocol (car parameters))
                                       (typename->protocol (car parameters)))
                       :make-with #'complex
                       :size 2))
      (make-instance 'fixed-size-container-type
                     :name typename
                     :contains '*
                     :make-with #'complex
                     :size 2)))
