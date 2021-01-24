(in-package :resample-examples)

(defun clamp (x)
  "Put X in [0,1] range"
  (declare (optimize (speed 3))
           (type single-float x))
  (min (max x 0f0) 1f0))

(defun convert-to-float (channel bps)
  (declare (optimize (speed 3))
           (type (simple-array (unsigned-byte 32)) channel)
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
                (floor (scale-float (clamp sample)
                                    (1- bps))))
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
               (channel2int
                (apply #'resample:resample
                       (channel2float (wav:decode-wav-data format channel))
                       (wav:format-samplerate format)
                       new-samplerate
                       resample-args)
                bps)))
        (let* ((data (mapcar #'resample-channel
                             (wav:read-wav-data reader format (wav:samples-num meta) :decompose t)))
               (new-nsamples (length (first data))))
          (utils:with-output-to-wav (out output-name
                                         :supersede t
                                         :samplerate new-samplerate
                                         :channels channels
                                         :bps bps
                                         :totalsamples new-nsamples)
            (write-sequence
             (utils:mixchannels
              (make-array (* new-nsamples channels) :element-type '(signed-byte 32))
              data)
             out))))))
  t)
