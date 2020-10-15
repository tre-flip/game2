;;;; classes.lisp

(in-package #:game2)

;; https://learnopengl.com/In-Practice/2D-Game/Collisions/Collision-detection

;;;;;;;;;;;;;;;;;;;;;;;;;
;; COLLISION INTERFACE ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric collide-p (a b)
  (:documentation "Checks if A and B collide."))

(defgeneric collide (a b)
  (:documentation "Performs actions when objects collide"))


;;;;;;;;;;;;;;;;;;;
;; BOX COLLISION ;;
;;;;;;;;;;;;;;;;;;;

;; TODO
(defclass collidable2-box ()
  ((height :initform 0 ;; 0 pixels
	   :initarg :collision-y
	   :accessor collision-y
	   :documentation "Used for collision detection.")
   (widht :initform 0 ;; 0 pixels
	  :initarg :collision-x
	  :accessor collision-x
	  :documentation "Used for collision detection."))
  (:documentation "Used to detect 2D collision. Assumes, that method DISTANCE is definde for this objcect"))


;;;;;;;;;;;;;;;;;;;;;;
;; CIRCLE COLLISION ;;
;;;;;;;;;;;;;;;;;;;;;;

(defclass collidable2-circle ()
  ((collision-radius :initform 0 ;; 0 pixels
		     :initarg :collision-radius
		     :accessor collision-radius
		     :documentation "Used for collision detection."))
  (:documentation "Used to detect 2D collision. Assumes, that method DISTANCE is definde for this objcect"))

;; collision for circle with circle
(defmethod collide-p ((a collidable2-circle) (b collidable2-circle))
  (< (+ (collision-radius a)
	(collision-radius b))
     (distance a b)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IN-GAME OBJECT INTERFACE ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric display (object)
  (:documentation "A method used to display according to its state."))

(defgeneric move (object delta)
  (:documentation "Move an object by adding delta to its coordinates."))

(defgeneric update (object)
  (:documentation "Update an object according to its state."))


;;;;;;;;;;;;;;;;;;;;;;;
;; INERTIA INTERFACE ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defclass inertial ()
  ((deceleration :initform 0.1)
   (stop-limit :initform 0.1)
   (inertia :initform (vec2 0 0)
	    :accessor inertia)))

(defmethod update :after ((object inertial))
  (with-slots (inertia deceleration stop-limit coords) object
    (if (and (> (abs (x inertia)) stop-limit)
	     (> (abs (y inertia)) stop-limit))
      (progn
	(setf coords (add coords inertia))
	(setf inertia (add inertia
			   (mult (inverse inertia) deceleration))))
      (setf inertia (vec2 0 0)))))

(defmethod move :after ((object inertial) (delta vec2))
  (with-slots (inertia inertia-koef inertia-koef-default) object
    (setf inertia (normalize delta))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PLAYER IMPLEMENTATION ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass player (collidable2-circle inertial)
  ((coords :initform (vec2 100 100)
	   :accessor coords)
   (collision-radius :initform 4)
   (heading-to :initform (vec2 0 0)
	       :accessor heading-to
	       :documentation "A vector that represents applied speed.")
   (speed-pixels :initform 3
		 :accessor speed-pixels))
  (:documentation "Represents player. Inherits X, Y from VEC2, collidable."))

(defmethod display ((player player))
  (draw-circle (coords player) 
	       (collision-radius player)
	       :fill-paint *color2*))

(defmethod move ((player player) (delta vec2))
  (with-slots (coords speed-pixels) player
    (let ((c coords)))
    (setf coords (add coords (mult (normalize delta)
				   speed-pixels)))
    (when (or (not (equal (y coords) (y c)))
	      (not (equal (x coords) (x c))))
      (print coords))))

(defmethod update ((player player))
  (move player
	(heading-to player)))
