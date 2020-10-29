;;;; classes.lisp

(in-package #:game2)

;; https://learnopengl.com/In-Practice/2D-Game/Collisions/Collision-detection

;;;;;;;;;;;;;;;;;;;;;;;;
;; KILLABLE INTERFACE ;;
;;;;;;;;;;;;;;;;;;;;;;;;
(defgeneric dead-p (obj))

(defclass killable ()
  ((dead :initform nil
	 :accessor dead)))

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

(defclass player (movable collidable2-circle killable)
  ((coords :initform (vec2 100 100)
	   :accessor coords)
   (collision-radius :initform 5)
   (heading :initform (vec2 0 0))
   (speed :initform 5)
   (hp :initform 3
       :accessor hp)
   (score :initform 0
	  :accessor score)
   (weapon :initform (make-instance 'player-bullet-spawner)
	   :accessor weapon))
  (:documentation "Represents player. Inherits X, Y from VEC2, collidable."))

(defmethod display ((player player))
  (draw-text "'()" (coords player)
	     :fill-color (color :aqua)
	     :font *font*))

(defmethod update ((player player))
  (with-slots (hp dead weapon) player
    (when (<= hp 0)
      (setf dead t)
      (setf weapon nil)))
  (move player))

(defmethod move-to ((player player) (where vec2))
  (setf (coords player)
	where))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; hp bonus implementation ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass hp-box (movable collidable2-circle killable)
  ((coords :initarg :coords
	   :accessor coords)
   (collision-radius :initform 5)
   (heading :initform (vec2 -1 0))
   (speed :initform 2)))

(defmethod update ((box hp-box))
  (move box))

(defmethod display ((box hp-box))
  (draw-text "<3"
	     (coords box)
	     :font *font*
	     :fill-color (color :pink)))


;;;;;;;;;;;;;;;;;;;;;;;;;
;; BOTS IMPLEMENTATION ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass bot (movable collidable2-circle killable spawner)
  ((coords :initform (vec2 *canvas-width*
			   (random *canvas-width*))
	   :initarg :coords)
   (heading :initform (vec2 -1 0)
	    :initarg :heading)
   (speed :initform (+ 1 (random 2)))
   (collision-radius :initform 5)
   (initial :initform (+ 60 (random 300)))
   (hp :initform (+ 1 (random 2)))))

(defmethod maybe-spawn ((bot bot))
  (make-instance 'bot-bullet
		 :heading (vec2 -1 0)
		 :coords (add (coords bot)
			      (vec2 -6 0))))

(defmethod update ((bot bot))
  (with-slots (coords heading hp dead) bot
    (when (<= hp 0)
      (setf dead t)
      (incf (score *player*)))
    (unless dead
      (move bot))
    (when (< (x coords) 0)
      (setf dead t))))

(defmethod display ((bot bot))
  (draw-text "@"
	     (coords bot)
	     :fill-color (color :red)
	     :font *font*))


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
	     :font *font*
	     :fill-color (color :grey)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BULLET IMPLEMENTATION ;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass bullet (movable killable collidable2-circle)
  ((coords :initarg :coords
	   :accessor coords)
   (heading :initarg :heading
	    :accessor heading)
   (collision-radius :initform 5)
   (speed :initform 6)))

(defmethod display ((bullet bullet))
  (draw-text "->"
	     (coords bullet)
	     :fill-color (color :pink)
	     :font *font*))

(defmethod update ((bullet bullet))
  (with-slots (coords heading dead) bullet
    (unless dead
      (move bullet))
    (when (> (x coords) *canvas-width*)
      (setf dead t))))

(defclass player-bullet (bullet) ())

(defclass bot-bullet (bullet) ())

(defmethod display ((bullet bot-bullet))
  (draw-text "<-"
	     (coords bullet)
	     :fill-color (color :red)
	     :font *font*))

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
    (decf initial (/ 1 (+ 1 (score *player*))))
    (make-instance 'bot
		   :coords (vec2 (+ x-start (random (+ 1 (- x-end x-start))))
				 (+ y-start (random (+ 1 (- y-end y-start))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; STAR SPAWNER IMPLEMENTATION ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass star-spawner (spawner)
  ((initial :initform 25)))

(defmethod maybe-spawn (obj) nil)

(defmethod maybe-spawn ((spawner star-spawner))
  (with-slots (x-start x-end y-start y-end cooldown initial delta) spawner
    (make-instance 'star
		   :coords (vec2 (+ x-start (random (+ 1 (- x-end x-start))))
				 (+ y-start (random (+ 1 (- y-end y-start))))))))



;;;;;;;;;;;;;;;;;;;;
;; BULLET SPAWNER ;;
;;;;;;;;;;;;;;;;;;;;

(defclass player-bullet-spawner (spawner)
  ((initial :initform 30)))

(defmethod maybe-spawn ((spawner player-bullet-spawner))
    (with-slots (x-start x-end y-start y-end cooldown initial delta) spawner
      (make-instance 'player-bullet
		     :heading (vec2 1 0)
		     :coords (add (coords *player*)
				  (vec2 6 0)))))

;;;;;;;;;;;;;;;;
;; HP SPAWNER ;; 
;;;;;;;;;;;;;;;;

(defclass hp-spawner (spawner)
  ((initial :initform 720)))

(defmethod maybe-spawn ((spawner hp-spawner))
  (with-slots (x-start x-end y-start y-end cooldown initial delta) spawner
    (make-instance 'hp-box
		   :coords (vec2 (+ x-start (random (+ 1 (- x-end x-start))))
				 (+ y-start (random (+ 1 (- y-end y-start))))))))


;;;;;;;;;;;;;;;;
;; COLLISIONS ;;
;;;;;;;;;;;;;;;;

(defmethod collide ((player player) (bot bot))
  (decf (hp *player*))
  (setf (dead bot) t))

(defmethod collide ((bot bot) (player player))
  (collide player bot))

(defmethod collide ((bot bot) (bot2 bot)))

(defmethod collide ((bullet player-bullet) (bot bot))
  (setf (dead bullet) t)
  (with-slots (hp) bot
    (decf hp)))

(defmethod collide ((bot bot) (bullet player-bullet))
  (collide bullet bot))

(defmethod collide ((player player) (bullet player-bullet))
  )

(defmethod collide ((bullet player-bullet) (player player))
  )

(defmethod collide ((bullet bot-bullet) (player player))
  (with-slots (hp) player
    (decf hp))
  (setf (dead bullet) t))

(defmethod collide ((player player) (bullet bot-bullet))
  (collide bullet player))

(defmethod collide ((bot-bullet bot-bullet) (player-bullet player-bullet))
  (setf (dead player-bullet) t
	(dead bot-bullet) t))

(defmethod collide ((bot-bullet bot-bullet) (bot bot))
  )

(defmethod collide ((bot bot) (bot-bullet bot-bullet))
  )

(defmethod collide ((player player) (hp-box hp-box))
  (with-slots (hp) player
    (incf hp)
    (with-slots (dead) hp-box
      (setf dead t))))

(defmethod collide ((hp-box hp-box) (player player))
  (collide player hp-box))
