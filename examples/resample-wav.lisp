(in-package :resample/examples)

(defun clamp (x min max)
  "Put X in [min, max] range"
  (declare (optimize (speed 3))
           (type (signed-byte 32) x min max))
  (min (max x min) max))

(defun convert-to-float (channel bps)
  (declare (optimize (speed 3))
           (type (simple-array (signed-byte 32)) channel)
           (type (integer 8 32) bps))
  (let ((new-channel (make-array (length channel)
                                 :element-type 'single-float)))
    (map-into new-channel
              (lambda (sample)
                (scale-float (float sample 0f0)
                             (- (1- bps))))
              channel)))

(defun convert-to-integer (channel bps)
  (declare (optimize (speed 3))
           (type (simple-array single-float) channel)
           (type (integer 8 32) bps))
  (let ((new-channel (make-array (length channel)
                                 :element-type '(signed-byte 32))))
    (map-into new-channel
              (lambda (sample)
                (clamp
                 (floor (+ (scale-float sample (1- bps))
                           (random 1f0)))
                 ( - (ash 1 (1- bps)))
                 (1- (ash 1 (1- bps)))))
              channel)))

(defun resample-wav (input-name output-name
                     new-samplerate &rest resample-args)
  "Recode WAV file with new sample rate. Additional arguments can be passed to RESAMPLE
in RESAMPLE-ARGS"
  (with-open-file (in input-name :element-type '(unsigned-byte 8))
    (let* ((reader (wav:open-wav in))
           (meta (wav:read-wav-header reader))
           (format (first meta))
           (channels (wav:format-channels-num format))
           (bps (wav:format-bps format)))
      (wav:reader-position-to-audio-data reader meta)
      (flet ((resample-channel (channel)
               (convert-to-integer
                (apply #'resample:resample
                       (convert-to-float (wav:decode-wav-data format channel) bps)
                       (wav:format-samplerate format)
                       new-samplerate
                       resample-args)
                bps)))
        (let* ((data (mapcar #'resample-channel
                             (wav:read-wav-data reader format (wav:samples-num meta) :decompose t)))
               (new-nsamples (length (first data))))
          (wav:with-output-to-wav (out output-name
                                       :supersede t
                                       :samplerate new-samplerate
                                       :channels channels
                                       :bps bps
                                       :totalsamples new-nsamples)
            (write-sequence
             (core:mixchannels
              (make-array (* new-nsamples channels) :element-type '(signed-byte 32))
              data)
             out))))))
  t)
