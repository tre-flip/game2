(in-package #:game2)

(defgeneric distance (a b)
  (:documentation  "distance between 2 vectors"))

(defmacro increase (field value)
  `(setf ,field
	 (add ,field ,value)))

(defmacro decrease (field value)
  `(setf ,field
	 (subt ,field ,value)))

(defmethod distance ((a vec2) (b vec2))
  (sqrt (+ (expt (- (x a) (x b)) 2)
	   (expt (- (y a) (y b)) 2))))

(defun display-palette ()
  "Test color palette."
  (fill-background *color1*)
  (draw-rect (vec2 100 100)
	     *canvas-width*
	     *canvas-height*
	     :fill-paint *color2*)
  (draw-rect (vec2 150 150)
	     *canvas-width*
	     *canvas-height*
	     :fill-paint *color3*)
  (draw-rect (vec2 200 200)
	     *canvas-width*
	     *canvas-height*
	     :fill-paint *color4*)
  (draw-rect (vec2 300 300)
	     *canvas-width*
	     *canvas-height*
	     :fill-paint *color5*))
