(in-package :resample-examples)

(defun channel2float (channel)
  (let ((new-channel (make-array (length channel) :element-type 'single-float)))
    (map-into new-channel
              (lambda (sample)
                (float sample 0f0))
              channel)))

(defun channel2int (channel bps)
  (let ((new-channel (make-array (length channel) :element-type '(signed-byte 32)))
        (min (- (ash 1 (1- bps))))
        (max (1- (ash 1 (1- bps)))))
    (map-into new-channel
              (lambda (sample)
                (max
                 (min max (truncate sample))
                 min))
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
                                         :supersede nil
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
