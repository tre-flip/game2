;;;; classes.lisp

(in-package #:game2)

;;;;;;;;;;;;;;;;;;;;;;;;;
;; COLLISION INTERFACE ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric collide-p (a b)
  (:documentation "Checks if A and B collide.")) 

(defmethod collide-p ((a standard-object) (b standard-object))
  nil)

(defclass collidable2 ()
  ((collision-radius :initform 0 ;; 0 pixels
		     :initarg :collision-radius
		     :accessor collision-radius
		     :documentation "Used for collision detection."))
  (:documentation "Used to detect 2D collision. Assumes, this object has X and Y coordinates!"))

(defmethod collide-p ((a collidable2) (b collidable2)))
