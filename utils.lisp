(in-package #:game2)

;;;;;;;;;;;;;;;;;;;;;;;;
;; CONVINIENCE MACROS ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro increase (field value)
  `(setf ,field
	 (add ,field ,value)))

(defmacro decrease (field value)
  `(setf ,field
	 (subt ,field ,value)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 2D DISTANCE INTERFACE ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric distance (a b)
  (:documentation  "distance between 2 vectors"))

(defmethod distance ((a vec2) (b vec2))
  (sqrt (+ (expt (- (x a) (x b)) 2)
	   (expt (- (y a) (y b)) 2))))


;;;;;;;;;;;;;;;;;;;;;;;
;; INVERSE INTERFACE ;;
;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric inverse (obj))

(defmethod inverse ((vec vec2))
  (vec2 (y vec) (x vec)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PALETTE FOR TESTING COLORS ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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


;;;;;;;;;;;;;
;; COUNTER ;;
;;;;;;;;;;;;;

(defgeneric counter-tick (arg))

(defgeneric counter-elapsed-p (arg))

(defgeneric counter-reset (counter &optional remains))

(defclass counter ()
  ((initial :initarg :initial
	    :initform 0)
   (delta :initform 1
	  :initarg delta)
   (remains)))

(defmethod initialize-instance :after ((counter counter) &rest initargs)
  (declare (ignore initargs))
  (with-slots (initial remains) counter
    (setf remains initial)))

(defmethod counter-tick ((counter counter))
  (with-slots (remains delta) counter
    (decf remains delta)))

(defmethod counter-elapsed-p ((counter counter))
  (with-slots (remains) counter
    (> remains 0)))

(defmethod counter-reset ((counter counter) &optional init)
  (with-slots (remains initial) counter
      (if init
	  (setf (remains counter) init)
	  (setf (remains counter) (initial counter)))))
