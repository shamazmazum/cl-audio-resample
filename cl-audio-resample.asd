(defsystem :cl-audio-resample
  :description "Audio resampling library"
  :serial t
  :version "1.0"
  :components ((:file "src/package")
               #+sbcl
               (:file "src/sbcl")
               (:file "src/resample")
               (:file "examples/package")
               (:file "examples/resample-wav"))
  :depends-on (:easy-audio))
