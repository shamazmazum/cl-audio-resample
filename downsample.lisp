(in-package cl-audio-downsample)

(declaim (type (double-float 0d0 1d0) *cutoff* *transition*))
(defvar *transition* (/ 7d0)
  "Width of transition region, must be < 1d0. Values from 1/20 to 1/7 are good.
Smaller values of *TRANSITION* require a filter of higher order.")
(defvar *cutoff* (/ 2d0)
  "How much of the original band to keep? Varies from 0d0 to 1d0")

(declaim (ftype (function (double-float &optional) double-float)
                filter-fourier-poly
                filter-fourier-trig-poly)
         (type (function (double-float &optional) double-float)
               *filter-function*))

(defun filter-fourier-trig-poly (w)
  "Fourier transform of low-pass filter, trigonometric polynomial variant"
  (declare (type double-float w))
  (let* ((eps (/ (* *cutoff* *transition*) 2d0))
         (x (/ (- w (- (* 0.5d0 *cutoff*) eps)) (* 2d0 eps))))
      (declare (type double-float eps x))
      (cond
        ((< w (/ (* *cutoff* (- 1d0 *transition*)) 2d0)) 1d0)
        ((> w (/ (* *cutoff* (+ 1d0 *transition*)) 2d0)) 0d0)
        (t
         (/
          (+
           (* -1d0 (cos (* 3 pi x)))
           (* 9d0 (cos (* pi x)))
           8d0)
          16d0)))))

(defun filter-fourier-poly (w)
  "Fourier transform of low-pass filter, polynomial variant"
  (declare (type double-float w))
  (let* ((eps (/ (* *cutoff* *transition*) 2d0))
         (x (/ (- w (- (* 0.5d0 *cutoff*) eps)) (* 2d0 eps))))
      (declare (type double-float eps x))
      (cond
        ((< w (/ (* *cutoff* (- 1d0 *transition*)) 2d0)) 1d0)
        ((> w (/ (* *cutoff* (+ 1d0 *transition*)) 2d0)) 0d0)
        (t
         (+
          (* 20d0 (expt x 7))
          (* -70d0 (expt x 6))
          (* 84d0 (expt x 5))
          (* -35d0 (expt x 4))
          1d0)))))

(defvar *filter-function* #'filter-fourier-poly
  "Which filter function to use?")

(defun get-fourier-coeff (n)
  "N-th coefficient of filter Fourier series decomposition"
  (declare (type integer n))
  (flet ((integrand (w n)
           (* (funcall *filter-function* w) (cos (* 2d0 pi n w)))))
    (let ((delta 1d-4)) ; Must depend on n really
      (* (/ delta 6) (if (zerop n) 2 4)
         (loop for w from 0d0 to (* *cutoff* 0.5d0 (1+ *transition*)) by delta sum
              (let ((x1 w)
                    (x2 (+ w delta))
                    (x3 (+ w (/ delta 2d0))))
                (+ (integrand x1 n)
                   (* 4d0 (integrand x3 n))
                   (integrand x2 n))))))))

(defun n-filter-coeffs (n)
  (declare (type (integer 1) n))
  (make-array n
              :element-type 'double-float
              :initial-contents
              (cons (get-fourier-coeff 0)
                    (loop for i from 1 below n collect
                         (/ (get-fourier-coeff i) 2d0)))))

(declaim (ftype (function (list) double-float) error-approx))
(defun error-approx (list)
  "Error approximation based on filter coefficients"
  (let* ((ratio
          (loop
             with a = 0d0
             with b = 0d0
             for c in (cdr list)
             for i from 1 by 1 do
               (incf a (* c (expt i 2) (cos (* pi i *cutoff* (1+ *transition*)))))
               (incf b (* c i (sin (* pi i *cutoff* (1+ *transition*)))))
             finally (return (/ b a))))
         (x (* (- (* *cutoff* (1+ *transition*)) (/ ratio pi)))))
    (loop
       for a in list
       for i from 0 by 1 sum
         (* a (cos (* pi i x))))))

(defun filter-coeffs-with-error (err &optional (min-coeffs 10) (max-steps 200))
  (declare (type double-float err))
  (labels ((repeat (list iteration)
             (if (or (< (abs (error-approx list)) err)
                     (>= iteration max-steps))
                 list (repeat (append list (list (get-fourier-coeff iteration)))
                              (1+ iteration)))))
    (let ((coeffs (repeat
                   (loop for i below min-coeffs collect (get-fourier-coeff i))
                   min-coeffs)))
      (make-array (length coeffs)
                  :element-type 'double-float
                  :initial-contents
                  (cons (car coeffs)
                        (mapcar (lambda (x) (/ x 2.0d0)) (cdr coeffs)))))))

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

(defun downsample (array ratio &key (filter-order 50 filter-order-given)
                                 (error 0d0 error-given)
                                 (minimal-order 10 minimal-order-given)
                                 (maximal-order 200 maximal-order-given)
                                 (filter :poly)
                                 (transition *transition*))
  "Downsample a signal ARRAY by a ratio RATIO. You can specify the filter order
in FILTER-ORDER or it can be calculated based on ERROR value. You can use two
filters: :POLY and :TRIG-POLY. Also you can specify a transition region TRANSITION.
Values about 1/7 are good."
  (declare (type (simple-array (double-float)) array)
           (type (integer 1) ratio)
           (type (integer 1) filter-order)
           (type (double-float 0d0) error)
           (type (member :poly :trig-poly) filter))
  (if (and filter-order-given
           (or error-given minimal-order-given maximal-order-given))
      (error
       "You cannot specify both FILTER-ORDER and any following parameter: ERROR, MINIMAL-ORDER, MAXIMAL-ORDER"))
  (let ((*cutoff* (float (/ ratio) 0d0))
        (*transition* transition)
        (*filter-function*
         (case filter
           (:poly #'filter-fourier-poly)
           (:trig-poly #'filter-fourier-trig-poly)
           (t #'filter-fourier-poly)))) ; to silence the compiler
    (convolve array (if error-given
                        (filter-coeffs-with-error error minimal-order maximal-order)
                        (n-filter-coeffs filter-order))
              ratio)))
