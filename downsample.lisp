(in-package cl-audio-downsample)

(declaim (type (double-float 0d0 1d0) *cutoff* *transition*))
(defvar *transition* (/ 7d0)
  "Width of transition region, must be < 1d0. Values from 1/20 to 1/7 are good.
Smaller values of *TRANSITION* require a filter of higher order.")
(defvar *cutoff* (/ 2d0)
  "How much of the original band to keep? Varies from 0d0 to 1d0")

(declaim (ftype (function (double-float &optional) double-float)
                transition-poly
                transition-trig-poly)
         (type (function (double-float &optional) double-float)
               *transition-function*))

(defun transition-trig-poly (w)
  "Transition region of low-pass filter, trigonometric polynomial variant"
  (declare (type double-float w))
  (/
   (+
    (* -1d0 (cos (* 3 pi w)))
    (* 9d0 (cos (* pi w)))
    8d0)
   16d0))

(defun transition-poly (w)
  "Transition region of low-pass filter, polynomial variant"
  (declare (type double-float w))
  (+
   (* 20d0 (expt w 7))
   (* -70d0 (expt w 6))
   (* 84d0 (expt w 5))
   (* -35d0 (expt w 4))
   1d0))

(defvar *transition-function* #'transition-poly
  "Which transition function to use?")

(defun integrate-function (func min max)
  "Integrate function f(x) from x = min to x = max"
  (declare (type double-float min max)
           (type (function (double-float &optional) double-float) func))
  (let ((delta 1d-4)) ; Must depend on n really
    (* (/ delta 6)
       (loop for x from min to max by delta sum
            (let ((x1 x)
                  (x2 (+ x delta))
                  (x3 (+ x (/ delta 2d0))))
              (+ (funcall func x1)
                 (* 4d0 (funcall func x3))
                 (funcall func x2)))))))

(defun get-filter-coeff (n)
  "N-th filter coefficient"
  (declare (type integer n))
  (if (zerop n) *cutoff*
      (flet ((integrand (w)
               (* (funcall *transition-function* w)
                  (cos (* 2d0 pi n
                          (+ (* *cutoff* *transition* w)
                             (* *cutoff* (- 1d0 *transition*) 0.5d0)))))))
        (+ (/ (sin (* *cutoff* (- 1 *transition*) n pi)) (* pi n))
           (* 2d0 *cutoff* *transition*
              (integrate-function #'integrand 0d0 1d0))))))

(defun n-filter-coeffs (n)
  (declare (type (integer 1) n))
  (make-array n
              :element-type 'double-float
              :initial-contents
              (loop for i below n collect
                   (get-filter-coeff i))))

(defun convolve (array flt &optional (drop-ratio 1))
  "Convolve two sequences"
  (declare (optimize (speed 3))
           (type (simple-array double-float) array flt)
           (type (integer 1 #.most-positive-fixnum) drop-ratio))
  (let* ((len1 (length array))
         (len2 (- (length flt) 1))
         (res-len (floor len1 drop-ratio))
         (new-array (make-array res-len
                                :initial-element 0d0
                                :element-type 'double-float)))
    (declare (type (simple-array double-float) new-array))
    (loop
       for i fixnum from 0 below res-len
       for array-center fixnum from 0 by drop-ratio
       do
         (loop for j from (- len2) to len2 do
              (let ((idx (+ array-center j)))
                (setq idx
                      (cond
                        ((< idx 0) (- idx))
                        ((>= idx len1) (- (* 2 len1) 1 idx))
                        (t idx)))
                (incf (aref new-array i)
                      (* (aref array idx)
                         (aref flt (abs j)))))))
    new-array))

(defun convolve (array flt &optional (drop-ratio 1))
  "Convolve two sequences"
  (declare (optimize (speed 3))
           (type (simple-array double-float) array flt)
           (type (integer 1 #.most-positive-fixnum) drop-ratio))
  (let* ((len1 (length array))
         (len2 (- (length flt) 1))
         (res-len (floor len1 drop-ratio))
         (new-array (make-array res-len
                                :initial-element 0d0
                                :element-type 'double-float)))
    (declare (type (simple-array double-float) new-array))
    (loop
       for i fixnum from 0 below res-len
       for array-center fixnum from 0 by drop-ratio
       do
         (loop for j from (- len2) to len2 do
              (let ((idx (+ array-center j)))
                (setq idx
                      (cond
                        ((< idx 0) (- idx))
                        ((>= idx len1) (- (* 2 len1) 1 idx))
                        (t idx)))
                (incf (aref new-array i)
                      (* (aref array idx)
                         (aref flt (abs j)))))))
    new-array))

(defun downsample (array ratio &key (filter-order 50)
                                 (transition-function :poly)
                                 (transition *transition*))
  "Downsample a signal ARRAY by a ratio RATIO. You can specify the
filter order in FILTER-ORDER. You can use filters with two
TRANSITION-FUNCTIONs: :POLY and :TRIG-POLY.  Also you can specify a
transition region TRANSITION. Values about 1/10 are good."
  (declare (type (simple-array (double-float)) array)
           (type (integer 1) ratio)
           (type (integer 1) filter-order)
           (type (member :poly :trig-poly) transition-function))
  (let ((*cutoff* (float (/ ratio) 0d0))
        (*transition* transition)
        (*transition-function*
         (ecase transition-function
           (:poly #'transition-poly)
           (:trig-poly #'transition-trig-poly))))
    (convolve array (n-filter-coeffs filter-order) ratio)))
