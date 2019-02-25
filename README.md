cl-audio-resample
===================

**cl-audio-resample** is an amateur audio resampler ;-)

You can resample your audio data (in the form of array of single-floats) like so:
~~~~~~{.lisp}
(cl-audio-resample:resample data old-samplerate new-samplerate)
~~~~~~

The first run is usually slow, as it takes time to calculate the low-pass filter.
I'll try to improve performance in the future.
