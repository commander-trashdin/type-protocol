


(in-package #:type-protocol)



(defmethod typename->protocol ((typename (eql '*)) &optional parameters)
  (declare (ignorable parameters))
  typename)
