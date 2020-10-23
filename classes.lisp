;;;; classes.lisp

(in-package #:game2)

;; https://learnopengl.com/In-Practice/2D-Game/Collisions/Collision-detection

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IN-GAME OBJECT INTERFACE ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric display (object)
  (:documentation "A method used to display according to its state."))

(defgeneric update (object)
  (:documentation "Update an object according to its state."))

;;;;;;;;;;;;;;;;;;;;;;;
;; PHYSICS INTERFACE ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric move (object)
  (:documentation "Move a object according to its state."))

(defgeneric move-to (obj destination)
  (:documentation "Moves an object to destination"))

(defgeneric move-by (obj delta)
  (:documentation "Moves an object by delta"))

;; default implementation
(defclass movable ()
  ((coords :initform (vec2 0 0))
   (speed :initform 0
	  :documentation "In pixels/frame.")
   (heading :initform (vec2 0 0)
	    :accessor heading)))

(defmethod move ((obj movable))
  (with-slots (coords speed heading) obj
    (setf coords (add coords
		      (mult (normalize heading)
			    speed)))))

(defmethod move-to ((obj movable) (delta vec2))
  )

(defmethod move-by ((obj movable) (delta vec2))
  )

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


;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PLAYER IMPLEMENTATION ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass player (movable collidable2-circle)
  ((coords :initform (vec2 100 100)
	   :accessor coords)
   (collision-radius :initform 4)
   (heading :initform (vec2 0 0))
   (speed :initform 6))
  (:documentation "Represents player. Inherits X, Y from VEC2, collidable."))

(defmethod display ((player player))
  (draw-text "'()" (coords player)
	     :fill-color *color2*))

(defmethod update ((player player))
  (move player))

(defmethod move-to ((player player) (where vec2))
  (setf (coords player)
	where))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; BOTS IMPLEMENTATION ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass bot (movable collidable2-circle)
  ((coords :initform (vec2 *canvas-width*
			   (random *canvas-width*)))
   (heading :initform (vec2 -1 0))
   (speed :initform (+ 1 (random 2)))
   (collision-radius :initform 5)))
