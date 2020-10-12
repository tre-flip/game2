;;;; game2.lisp

(in-package #:game2)

;; canvas dimentions
(defvar *canvas-width* 800)
(defvar *canvas-height* 600)

(defvar *origin* (gamekit:vec2 0 0))

;; color palette
(defvar *very-dark* (vec4 0.02 0.28 0.30 1))
(defvar *dark*      (vec4 0 0.68 0.69 1))
(defvar *normal*    (vec4 0.44 0.120 0.115 1))
(defvar *light*     (vec4 0.111 0.185 0.143 1))

(defgame *game* () ()
  (:viewport-width *canvas-width*)	    ; window's width
  (:viewport-height *canvas-height*)	    ; window's height
  (:viewport-title "treflip's tomfoolery"))  ; window's title

(defun main ()
  (start '*game*))

(defun fill-background (color)
  ;; (draw-rect *origin*  *canvas-width* *canvas-height* :fill-paint color)
  (draw-rect *origin*  *canvas-width* *canvas-height* :fill-paint color))

(defun display-palette ()
  "Test color palette."
  (fill-background *very-dark*)
  (draw-rect (vec2 100 100)
	     *canvas-width*
	     *canvas-height*
	     :fill-paint *dark*)
  (draw-rect (vec2 150 150)
	     *canvas-width*
	     *canvas-height*
	     :fill-paint *normal*)
  (draw-rect (vec2 200 200)
	     *canvas-width*
	     *canvas-height*
	     :fill-paint *light*))

(defmethod post-initialize ((ap *game*))
  )

(defmethod act ((app *game*))
  )

(defmethod draw ((app *game*))
  )
