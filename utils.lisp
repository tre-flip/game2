(in-package #:game2)

(defgeneric distance (a b)
  (:documentation  "distance between 2 vectors"))

(defmethod distance ((a vec2) (b vec2))
   (sqrt (+ (expt (- (x a) (x b)) 2)
		(expt (- (y a) (y b)) 2))))
