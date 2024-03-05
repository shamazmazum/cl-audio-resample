(defsystem :cl-audio-resample
  :description "Audio resampling library"
  :serial t
  :version "1.0"
  :pathname "src"
  :components ((:file "package")
               (:file "resample")))

(defsystem :cl-audio-resample/examples
  :description "Audio resampling library (examples)"
  :serial t
  :version "1.0"
  :pathname "examples"
  :components ((:file "package")
               (:file "resample-wav"))
  :depends-on (:easy-audio :cl-audio-resample))
