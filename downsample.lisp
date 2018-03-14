(in-package cl-audio-downsample)

(declaim (type (double-float 0d0 1d0) *a* *b*))
(defvar *b* (/ 7d0)
  "Width of transition region")
(defvar *a* (/ 2d0)
  "How much of the original band to keep? Varies from 0 to 1")

(declaim (ftype (function (double-float &optional) double-float)
                filter-fourier-poly4
                filter-fourier-poly10)
         (type (function (double-float &optional) double-float)
               *filter-function*))
(defun filter-fourier-poly4 (w)
  "Fourier transform of low-pass filter, variant 1"
  (declare (type double-float w))
  (let ((eps (/ (* *a* *b*) 2d0))
        (x (- w (/ (* *a* (- 1d0 *b*)) 2d0))))
    (declare (type double-float eps x))
    (cond
      ((< w (/ (* *a* (- 1d0 *b*)) 2d0)) 1d0)
      ((> w (/ (* *a* (+ 1d0 *b*)) 2d0)) 0d0)
      (t
       (+ (expt (/ x 2d0 eps) 4d0)
          (- (* 2d0 (expt (/ x 2d0 eps) 2d0)))
          1d0)))))

(defun filter-fourier-poly10 (w)
  "Fourier transform of low-pass filter, variant 2"
  (declare (type double-float w))
  (let* ((eps (/ (* *a* *b*) 2d0))
         (x (/ (- w (- (* 0.5d0 *a*) eps)) (* 2d0 eps))))
      (declare (type double-float eps x))
      (cond
        ((< w (/ (* *a* (- 1d0 *b*)) 2d0)) 1d0)
        ((> w (/ (* *a* (+ 1d0 *b*)) 2d0)) 0d0)
        (t
         (+
          (* 4d0 (expt x 10))
          (- (* 15d0 (expt x 8)))
          (* 20d0 (expt x 6))
          (- (* 10d0 (expt x 4)))
          1d0)))))

(defvar *filter-function* #'filter-fourier-poly10
  "Which filter function to use?")

(defun get-fourier-coeff (n)
  "N-th coefficient of filter Fourier series decomposition"
  (declare (type integer n))
  (flet ((integrand (w n)
           (* (funcall *filter-function* (abs w)) (cos (* 2d0 pi n w)))))
    (let ((delta 1d-4)) ; Must depend on n really
      (* (/ delta 6) (if (zerop n) 1 2)
         (loop for w from (- (* *a* 0.5d0 (1+ *b*))) to (* *a* 0.5d0 (1+ *b*)) by delta sum
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
               (incf a (* c (expt i 2) (cos (* pi i *a* (1+ *b*)))))
               (incf b (* c i (sin (* pi i *a* (1+ *b*)))))
             finally (return (/ b a))))
         (x (* (- (* *a* (1+ *b*)) (/ ratio pi)))))
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

(defun filter (array flt)
  "Convolve filter with the signal"
  (declare (optimize (speed 3))
           (type (simple-array double-float) array flt))
  (let* ((len1 (length array))
         (len2 (- (length flt) 1))
         (new-array (make-array len1
                                :initial-element 0d0
                                :element-type 'double-float)))
    (declare (type (simple-array double-float) new-array))
    (loop for i from 0 below len1 do
         (loop for j from (- len2) to len2 do
              (let ((idx (+ i j)))
                (setq idx
                      (cond
                        ((< idx 0) (- idx))
                        ((>= idx len1) (- (* 2 len1) 1 idx))
                        (t idx)))
                (incf (aref new-array i)
                      (* (aref array idx)
                         (aref flt (abs j)))))))
    new-array))

(defun drop-samples (array n)
  "Retain every N-th sample"
  (declare (type (simple-array (double-float)) array)
           (type (integer 1 #.most-positive-fixnum) n)
           (optimize (speed 3)))
  (let* ((len (floor (length array) n))
         (new-array (make-array len :element-type 'double-float)))
    (declare (type (simple-array (double-float)) new-array))
    (loop
       for idx1 fixnum below len
       for idx2 fixnum from 0 by n
       do
         (setf (aref new-array idx1)
               (aref array idx2)))
    new-array))

(defun downsample (array ratio &key (filter-order 50 filter-order-given)
                                 (error 0d0 error-given)
                                 (minimal-order 10 minimal-order-given)
                                 (maximal-order 200 maximal-order-given)
                                 (filter :poly10)
                                 (transition *b*))
  "Downsample a signal ARRAY by a ratio RATIO. You can specify the filter order
in FILTER-ORDER or it can be calculated based on ERROR value. You can use two
filters: :POLY4 and :POLY10. Also you can specify a transition region TRANSITION.
Values about 1/7 are good."
  (declare (type (simple-array (double-float)) array)
           (type (integer 1) ratio)
           (type (integer 1) filter-order)
           (type (double-float 0d0) error)
           (type (member :poly10 :poly4) filter))
  (if (and filter-order-given
           (or error-given minimal-order-given maximal-order-given))
      (error
       "You cannot specify both FILTER-ORDER and any following parameter: ERROR, MINIMAL-ORDER, MAXIMAL-ORDER"))
  (let ((*a* (float (/ ratio) 0d0))
        (*b* transition)
        (*filter-function*
         (case filter
           (:poly10 #'filter-fourier-poly10)
           (:poly4 #'filter-fourier-poly4)
           (t #'filter-fourier-poly10)))) ; to silence the compiler
    (drop-samples
     (filter array (if error-given
                       (filter-coeffs-with-error error minimal-order maximal-order)
                       (n-filter-coeffs filter-order)))
     ratio)))
