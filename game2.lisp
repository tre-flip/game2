;;;; game2.lisp

;;  GAME LOOP

(in-package #:game2)

;; resources
(register-resource-package :assets (asdf:system-relative-pathname :game2 "assets/"))
(define-font assets::fixed-sys "fixed-sys")
(defparameter *font* nil)
(defparameter *font-big* nil)

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

(defparameter *hp-spawner* (make-instance 'hp-spawner
					   :x-start *canvas-width*
					   :x-end *canvas-width*
					   :y-start 0
					   :y-end *canvas-height*))

(defparameter *objects* (list *player*)
  "An object pool")

(defmethod post-initialize ((ap *game*))
  (setf *font* (make-font 'assets::fixed-sys 16))
  (setf *font-big* (make-font 'assets::fixed-sys 30))
  (bind-up    :pressed (lambda () (increase (heading *player*) (vec2 0 1))))
  (bind-left  :pressed (lambda () (increase (heading *player*) (vec2 -1 0))))
  (bind-down  :pressed (lambda () (increase (heading *player*) (vec2 0 -1))))
  (bind-right :pressed (lambda () (increase (heading *player*) (vec2 1 0))))

  (bind-up    :released (lambda () (increase (heading *player*) (vec2 0 -1))))
  (bind-left  :released (lambda () (increase (heading *player*) (vec2 1 0))))
  (bind-down  :released (lambda () (increase (heading *player*) (vec2 0 1))))
  (bind-right :released (lambda () (increase (heading *player*) (vec2 -1 0))))  
  )

(defun draw-hud (origin)
  (with-slots (hp score) *player*
    (draw-text (format nil "HP: ~a SCORE: ~a" hp score)
	       origin
	       :fill-color (color :white)
	       :font *font*)))

(defun draw-end ()
  (with-slots (score) *player*
      (draw-text (format nil "GAME OVER, SCORE: ~a" score)
		 (vec2 260 (/ *canvas-height* 2.0))
		 :fill-color (color :white)
		 :font *font-big*)))


(defmethod act ((app *game*))

  (awhen (maybe-spawn *hp-spawner*)
    (push it *objects*))
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
	do (progn
	     (update obj)
	     (awhen (maybe-spawn obj)
	       (push it *objects*)))))

(defmethod draw ((app *game*))
  (fill-background (color :black))
  ;; display each object
  (loop for obj in *objects*
	do (display obj))
  (draw-hud (vec2 10 10))
  (when (dead-p *player*)
    (draw-end)))
