(defpackage cl-audio-resample/examples
  (:nicknames #:resample/examples)
  (:use #:cl)
  (:local-nicknames (#:wav   #:easy-audio.wav)
                    (#:core #:easy-audio.core))
  (:export #:resample-wav))
