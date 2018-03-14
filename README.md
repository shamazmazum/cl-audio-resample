cl-audio-downsample
===================

**cl-audio-downsample** is a strange audio downsampler. It was written by a man who knows nothing
about DSP and yet, it's somewhat comparable to FFmpeg's swr (which is also not so good). It also has
can only convert to a sampling rate which is in integral relation with the old one
(e.g. 192kHz->48kHz or 96kHz->48kHz, but 96kHz->44.1kHz will not work). It's an implementational
restriction. You can find a mathematical explanation of my downsampler and some comparison data in
the attached pdf.
