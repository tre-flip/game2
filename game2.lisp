;;;; game2.lisp

;;  GAME LOOP

(in-package #:game2)

;; resources
(register-resource-package :game (asdf:system-relative-pathname :game2 "assets/"))
(define-font fixed-sys "assets/fixed-sys")
;; canvas dimentions
(defvar *canvas-width* 800)
(defvar *canvas-height* 600)

(defvar *origin* (gamekit:vec2 0 0))

(defgame *game* () ()
  (:draw-rate 60)
  (:act-rate 60)
  (:prepare-resources t)
  (:viewport-width *canvas-width*)	    ; window's width
  (:viewport-height *canvas-height*)	    ; window's height
  (:viewport-title "treflip's tomfoolery"))  ; window's title

(defun main ()
  (start '*game*))

(defun fill-background (color)
  ;; (draw-rect *origin*  *canvas-width* *canvas-height* :fill-paint color)
  (draw-rect *origin*  *canvas-width* *canvas-height* :fill-paint color))

(defparameter *player* (make-instance 'player))

(defparameter *star-spawner* (make-instance 'star-spawner
					   :x-start *canvas-width*
					   :x-end *canvas-width*
					   :y-start 0
					   :y-end *canvas-height*))

(defparameter *bot-spawner* (make-instance 'bot-spawner
					   :x-start *canvas-width*
					   :x-end *canvas-width*
					   :y-start 0
					   :y-end *canvas-height*))
(defparameter *objects* (list *player*)
  "An object pool")

(defmethod post-initialize ((ap *game*))
  (bind-up    :pressed (lambda () (increase (heading *player*) (vec2 0 1))))
  (bind-left  :pressed (lambda () (increase (heading *player*) (vec2 -1 0))))
  (bind-down  :pressed (lambda () (increase (heading *player*) (vec2 0 -1))))
  (bind-right :pressed (lambda () (increase (heading *player*) (vec2 1 0))))

  (bind-up    :released (lambda () (increase (heading *player*) (vec2 0 -1))))
  (bind-left  :released (lambda () (increase (heading *player*) (vec2 1 0))))
  (bind-down  :released (lambda () (increase (heading *player*) (vec2 0 1))))
  (bind-right :released (lambda () (increase (heading *player*) (vec2 -1 0))))  
  )

(defmethod act ((app *game*))
  (awhen (maybe-spawn *star-spawner*)
    ;; make it add 3 layers of scene!
    (push it *objects*))
  (awhen (maybe-spawn *bot-spawner*)
    ;; make it push to the second layer
    (push it *objects*))
  (awhen (maybe-spawn (weapon *player*))
    ;; make it push to the second layer
    (push it *objects*))
  (handler-case
      (alexandria:map-combinations (lambda (comb) 
				     (when (collide-p (car comb) (cadr comb))
				       (collide (car comb) (cadr comb))))
				   *objects*
				   :length 2
				   :copy nil)
    (t (arg) (print arg))) 
  (loop for obj in *objects*
	when (dead-p obj)
	  do (setf *objects* (delete obj *objects*)) 
	do (update obj)))

(defmethod draw ((app *game*))
  (fill-background (color :black))
  ;; display each object
  (loop for obj in *objects*
	do (display obj)))
