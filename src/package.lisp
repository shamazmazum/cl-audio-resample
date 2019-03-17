(defpackage cl-audio-resample
  (:use #:cl)
  (:nicknames #:resample)
  (:export #:resample
           #:*transition*
           #:*cutoff*))
