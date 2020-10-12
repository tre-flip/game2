;;;; classes.lisp

(in-package #:game2)

(defclass collidable ()
  ((collision-radius :initform 5
		     :initarg :collision-radius
		     :accessor collision-radius
		     :documentation "Used for collision detection."))
  (:documentation "Collidable objects can collide with each other. Assumes, this object has X and Y coordinates!"))
