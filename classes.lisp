;;;; classes.lisp

(in-package #:game2)

;; https://learnopengl.com/In-Practice/2D-Game/Collisions/Collision-detection

;;;;;;;;;;;;;;;;;;;;;;;;
;; KILLABLE INTERFACE ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric dead-p (obj))

(defclass killable ()
  ((dead :initform nil)))

(defmethod dead-p ((obj killable))
  (with-slots (dead) obj
    (when dead t)))

(defmethod dead-p (obj)
  nil)

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
  ((coords :initform (vec2 0 0)
	   :accessor coords)
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

(defmethod collide-p (a b))

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
  (> (+ (collision-radius a)
	(collision-radius b))
     (distance (coords a)
	       (coords b))))


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
	     :fill-color (color :teal)))

(defmethod update ((player player))
  (move player))

(defmethod move-to ((player player) (where vec2))
  (setf (coords player)
	where))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; BOTS IMPLEMENTATION ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass bot (movable collidable2-circle killable)
  ((coords :initform (vec2 *canvas-width*
			   (random *canvas-width*))
	   :initarg :coords)
   (heading :initform (vec2 -1 0)
	    :initarg :heading)
   (speed :initform (+ 1 (random 2)))
   (collision-radius :initform 5)
   (hp :initform (+ 1 (random 2)))))


(defmethod update ((bot bot))
  (with-slots (coords heading hp dead) bot
    (when (< hp 0)
      (setf dead t))
    (unless dead
      (move bot))
    (when (< (x coords) 0)
      (setf dead t))))

(defmethod display ((bot bot))
  (draw-text "@"
	     (coords bot)
	     :fill-color (color :maroon)))


;;;;;;;;;;;;;;;;;;;;;;;;;;
;; STARS IMPLEMENTATION ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass star (movable killable)
  ((coords :initform (vec2 *canvas-width*
			   (random *canvas-width*))
	   :initarg :coords)
   (heading :initform (vec2 -1 0)
	    :initarg :heading)
   (speed :initform (+ 1 (random 2)))))

(defmethod update ((star star))
  (with-slots (coords heading dead) star
    (unless dead
      (move star))
    (when (< (x coords) 0)
      (setf dead t))))

(defmethod display ((star star))
  (draw-text "*"
	     (coords star)
	     :fill-color (color :grey)))

;;;;;;;;;;;;;;;;;;;;;;;
;; SPAWNER INTERFACE ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric maybe-spawn (spawner)
  (:documentation "Indeterministically spawns an object."))

(defclass spawner (counter)
  ((initial :initform 100)
   (remains :initform 5)
   (x-start :initarg :x-start)
   (x-end :initarg :x-end)
   (y-start :initarg :y-start)
   (y-end :initarg :y-end)))

(defmethod maybe-spawn :around ((spawner spawner))
  (with-slots (x-start x-end y-start y-end cooldown initial delta) spawner
    (counter-tick spawner)
    (when (counter-elapsed-p spawner)
      (progn
	(counter-reset spawner)
	(call-next-method)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BOT SPAWNER IMPLEMENTATION ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass bot-spawner (spawner) ())

(defmethod maybe-spawn ((spawner bot-spawner))
  (with-slots (x-start x-end y-start y-end cooldown initial delta) spawner
    (make-instance 'bot
		   :coords (vec2 (+ x-start (random (+ 1 (- x-end x-start))))
				 (+ y-start (random (+ 1 (- y-end y-start))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; STAR SPAWNER IMPLEMENTATION ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass star-spawner (spawner)
  ((initial :initform 25)))

(defmethod maybe-spawn ((spawner star-spawner))
  (with-slots (x-start x-end y-start y-end cooldown initial delta) spawner
    (make-instance 'star
		   :coords (vec2 (+ x-start (random (+ 1 (- x-end x-start))))
				 (+ y-start (random (+ 1 (- y-end y-start))))))))


;;;;;;;;;;;;;;;;
;; COLLISIONS ;;
;;;;;;;;;;;;;;;;

(defmethod collide ((player player) (bot bot))
  (print "COLLISION"))

(defmethod collide ((bot bot) (player player))
  (print "COLLISION"))

(defmethod collide ((bot bot) (bot2 bot))
  (print "COLLISION"))
