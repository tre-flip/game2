;;;; game2.lisp

;;  GAME LOOP

(in-package #:game2)

;; canvas dimentions
(defvar *canvas-width* 800)
(defvar *canvas-height* 600)

(defvar *origin* (gamekit:vec2 0 0))

;; color palette
;; from neutral to most noticable
(defparameter *color1* (vec4 0.02 0.28 0.30 1))
(defparameter *color2*      (vec4 0 0.68 0.69 1))
(defparameter *color3*    (vec4 0.44 0.120 0.115 1))
(defparameter *color4*     (vec4 0.55 0.2 0.3 1))
(defparameter *color5*     (vec4 0.55 0.1 0.3 1))

(defgame *game* () ()
  (:viewport-width *canvas-width*)	    ; window's width
  (:viewport-height *canvas-height*)	    ; window's height
  (:viewport-title "treflip's tomfoolery"))  ; window's title

(defun main ()
  (start '*game*))

(defun fill-background (color)
  ;; (draw-rect *origin*  *canvas-width* *canvas-height* :fill-paint color)
  (draw-rect *origin*  *canvas-width* *canvas-height* :fill-paint color))

(defparameter *objects* nil
  "An object pool")

(defmethod post-initialize ((ap *game*))
  )

(defmethod act ((app *game*))
  )

(defmethod draw ((app *game*))
  (fill-background *color1*)
  ;; display each object
  (loop for obj in *objects*
	do (display obj)))
