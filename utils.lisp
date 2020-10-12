(in-package #:game2)

(defgeneric distance (a b)
  (:documentation  "distance between 2 vectors"))

(defmethod distance ((a vec2) (b vec2))
   (sqrt (+ (expt (- (x a) (x b)) 2)
	    (expt (- (y a) (y b)) 2))))

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
