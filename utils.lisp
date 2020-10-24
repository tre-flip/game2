(in-package #:game2)

;;;;;;;;;;;;;;;;;;;;;;;;
;; CONVINIENCE MACROS ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro increase (field value)
  "operates on vectors"
  `(setf ,field
	 (add ,field ,value)))

(defmacro decrease (field value)
  "operates on vectors"  
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
  (vec2 (- (x vec))
	(- (y vec))))


;;;;;;;;;;;;;
;; PALETTE ;;
;;;;;;;;;;;;;

(defparameter *palette* (alexandria:plist-hash-table (list :black (vec4 0 0 0 1)
							   :grey (vec4 0.75 0.75 0.75 0.75)
							   :pink (vec4 1 0 1 0.4)
							   :maroon (vec4 0.5 0 0 0.7)
							   :teal (vec4 0 0.5 0.5 0.7)))
  "color palette")

(defun color (key)
  (gethash key *palette*))

;;;;;;;;;;;;;
;; COUNTER ;;
;;;;;;;;;;;;;

(defgeneric counter-tick (arg))

(defgeneric counter-elapsed-p (arg))

(defgeneric counter-reset (counter &optional remains))

(defclass counter ()
  ((initial :initarg :initial
	    :accessor initial
	    :initform 0)
   (delta :initform 1
	  :initarg :delta)
   (remains :accessor remains)))

(defmethod initialize-instance :after ((counter counter) &rest initargs)
  (declare (ignore initargs))
  (with-slots (initial remains) counter
    (setf remains initial)))

(defmethod counter-tick ((counter counter))
  (with-slots (remains delta) counter
    (decf remains delta)))

(defmethod counter-elapsed-p ((counter counter))
  (with-slots (remains) counter
    (<= remains 0)))

(defmethod counter-reset ((counter counter) &optional init)
  (with-slots (remains initial) counter
      (if init
	  (setf (remains counter) init) 
	  (setf (remains counter) (initial counter)))))

