(in-package cl-audio-resample)

(deftype range-01 () '(single-float 0.0 1.0))
(deftype positive-fixnum () '(integer 1 #.most-positive-fixnum))

(declaim (type range-01 *cutoff* *transition*)
         (type positive-fixnum *integration-steps*)
         (type single-float +single-pi+))
(defparameter *transition* (/ 15.0)
  "Width of transition region, must be < 1d0. Values from 1/20 to 1/7 are good.
Smaller values of *TRANSITION* require a filter of higher order.")
(defparameter *cutoff* (/ 2.0)
  "How much of the original band to keep? Varies from 0d0 to 1d0")
(defparameter *integration-steps* 250
  "Number of integration steps in filter fourier decomposition")
(defconstant +single-pi+ (float pi 0.0)
  "Single float pi constant")
(defvar *filter-bank-table* (make-hash-table :test #'equalp)
  "Filter bank memo")

(declaim (type (function (single-float &optional) single-float)
               *transition-function*))

(defun transition-trig-poly (w)
  "Transition region of low-pass filter, trigonometric polynomial variant"
  (declare (type single-float w)
           (optimize (speed 3)))
  (/
   (+
    (* -1 (cos (* 3 +single-pi+ w)))
    (* 9 (cos (* +single-pi+ w)))
    8)
   16))

(defun calculate-polynomial (x &rest coeff)
  "Calculate polynomial over X with gives single-float coefficients COEFF"
  (declare (optimize (speed 3))
           (type single-float x))
  (flet ((reduce-poly (acc c)
           (declare (type single-float acc c))
           (+ (* x acc) c)))
  (reduce #'reduce-poly coeff)))

(defun transition-poly (w)
  "Transition region of low-pass filter, better convergence polynomial variant"
  (calculate-polynomial w 20.0 -70.0 84.0 -35.0 0.0 0.0 0.0 1.0))

(defun transition-poly-fast (w)
  "Transition region of low-pass filter, faster calculation polynomial variant"
  (calculate-polynomial w 2.0 -3.0 0.0 1.0))

(defvar *transition-function* #'transition-poly
  "Which transition function to use?")

(defun integrate-function (func min max)
  "Integrate function f(x) from x = min to x = max"
  (declare (type single-float min max)
           (type (function (single-float &optional) single-float) func)
           (optimize (speed 3)))
  (let ((delta (float (/ *integration-steps*) 0.0)))
    (declare (type single-float delta))
    (* (/ delta 6)
       (loop for x from min to max by delta sum
            (let ((x1 x)
                  (x2 (+ x delta))
                  (x3 (+ x (/ delta 2))))
              (+ (funcall func x1)
                 (* 4 (funcall func x3))
                 (funcall func x2)))
            single-float))))

(defun get-filter-coeff (n)
  "N-th filter coefficient"
  (declare (type (unsigned-byte 32) n)
           (optimize (speed 3)))
  (if (zerop n) *cutoff*
      (flet ((integrand (w)
               (* (funcall *transition-function* w)
                  (cos (* 2 +single-pi+ n
                          (+ (* *cutoff* *transition* w)
                             (* *cutoff* (- 1 *transition*) 0.5)))))))
        (+ (/ (sin (* *cutoff* (- 1 *transition*) n +single-pi+)) (* +single-pi+ n))
           (* 2 *cutoff* *transition*
              (integrate-function #'integrand 0.0 1.0))))))

(defun filter-bank (phases length)
  "Calculate PHASESxLENGTH multiphase low-pass filter bank"
  (declare (type unsigned-byte length phases))
  (let* ((total-length (* phases length))
         ;; Our filter length is odd
         (max-idx (1- (logand (1+ total-length) -2)))
         (linear-length (/ (1+ max-idx) 2))
         (filter (make-array (list phases length)
                             :element-type 'single-float)))
    (loop for i below phases do
         (loop for j below length do
              (setf (aref filter i j)
                    (let ((idx (+ (* j phases) i)))
                      (if (< idx max-idx)
                          (get-filter-coeff (abs (- idx (1- linear-length))))
                          0.0)))))
    filter))

(defstruct filter-bank-id
  (phases 1 :type positive-fixnum)
  (length 1 :type positive-fixnum)
  (cutoff 0.0 :type single-float)
  (transition 0.0 :type single-float)
  (transition-fn #'identity :type function))

(defun get-filter-bank (phases length)
  (let ((filter-id (make-filter-bank-id :phases phases
                                        :length length
                                        :cutoff *cutoff*
                                        :transition *transition*
                                        :transition-fn *transition-function*)))
  (multiple-value-bind (filter filter-found-p)
      (gethash filter-id
               *filter-bank-table*)
    (cond
      (filter-found-p filter)
      (t (setf (gethash filter-id *filter-bank-table*)
               (filter-bank phases length)))))))

(defun resample-with-filtering (array up down filter-length)
  "Perform audio resampling with low-pass filtering."
  (declare (optimize (speed 3))
           (type (simple-array single-float (*)) array)
           (type positive-fixnum up down filter-length))
  (let* ((in-length (length array))
         (out-length (floor (* in-length up) down))
         (output (make-array out-length
                             :element-type 'single-float))
         (filter (get-filter-bank up filter-length)))
    (declare (type (simple-array single-float (* *)) filter))
    (loop for i fixnum below out-length do
         (multiple-value-bind (input-idx filter-idx)
             (floor (* i down) up)
           (declare (type (integer 0 #.most-positive-fixnum) input-idx filter-idx))
           (setf (aref output i)
                 (* up
                    (loop
                      for j fixnum below filter-length
                      for idx fixnum = (- input-idx j)
                      sum
                      (*
                       (aref filter filter-idx j)
                       (if (< idx 0) 0.0 (aref array idx)))
                      single-float)))))
    output))

(defun up-down (old-samplerate new-samplerate)
  (declare (type positive-fixnum old-samplerate new-samplerate))
  (let ((ratio (/ new-samplerate old-samplerate)))
    (values (numerator ratio)
            (denominator ratio))))

(defun resample (array old-samplerate new-samplerate
                 &key
                   (filter-length 250)
                   (transition-function :trig-poly)
                   (transition *transition*))
  "Perform resampling of audio data in ARRAY from OLD-SAMPLERATE to
NEW-SAMPLERATE. Higher value of FILTER-LENGTH usually gives a better
result but higher computation time. Smaller TRANSITION cuts
frequencies which are just above cutoff frequency more effectively but
requires higher FILTER-LENGTH. Usually, default values for keyed
arguments are OK."
  (declare (type (simple-array single-float (*)) array)
           (type positive-fixnum
                 old-samplerate
                 new-samplerate
                 filter-length)
           (type single-float transition))

  (multiple-value-bind (up down)
      (up-down old-samplerate new-samplerate)
    (let ((*transition* transition)
          (*cutoff* (float (/ (max up down)) 0.0))
          (*transition-function*
           (ecase transition-function
             (:poly #'transition-poly)
             (:poly-fast #'transition-poly-fast)
             (:trig-poly #'transition-trig-poly))))
      (resample-with-filtering array up down filter-length))))
