;;;; classes.lisp

(in-package #:game2)

;;;;;;;;;;;;;;;;;;;;;;;;;
;; COLLISION INTERFACE ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

;; https://learnopengl.com/In-Practice/2D-Game/Collisions/Collision-detection

(defgeneric collide-p (a b)
  (:documentation "Checks if A and B collide.")) 

(defmethod collide-p ((a standard-object) (b standard-object))
  nil)

(defclass collidable2-box ()
  ((collision-y :initform 0 ;; 0 pixels
		:initarg :collision-y
		:accessor collision-y
		:documentation "Used for collision detection.")
   (collision-x :initform 0 ;; 0 pixels
		:initarg :collision-x
		:accessor collision-x
		:documentation "Used for collision detection."))
  (:documentation "Used to detect 2D collision. Assumes, this object has X and Y coordinates!"))

(defclass collidable2-circle ()
  ((collision-radius :initform 0 ;; 0 pixels
		     :initarg :collision-radius
		     :accessor collision-radius
		     :documentation "Used for collision detection."))
  (:documentation "Used to detect 2D collision. Assumes, this object has X and Y coordinates!"))
