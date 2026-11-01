;;; beatmacs-synth.el --- Pure Elisp drum & bass synthesis -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Georg

;; This file is not part of GNU Emacs.

;;; Commentary:

;; A tiny synthesizer written in plain Emacs Lisp.  No external
;; programs are needed, so it works on Emacs for Android as well.
;;
;; Everything here works on vectors of floats in the range -1.0..1.0
;; (one float per sample, mono).  The building blocks are:
;;
;;   oscillators  sine, saw, square, noise     -> the raw "tone"
;;   envelopes    exponential decay, AR gate   -> how loud over time
;;   filters      one-pole high-pass, state-variable low/band-pass
;;                                              -> remove/emphasize freqs
;;
;; Each instrument ("voice") combines these.  A kick drum, for
;; example, is a sine wave whose pitch falls quickly from ~150 Hz to
;; ~50 Hz while its volume decays: that pitch drop is the "thump".
;;
;; The mixer takes a list of events (when, which voice, how loud),
;; renders each distinct voice once (cached), and adds it into a loop
;; buffer.  Tails that run past the end of the loop wrap around to the
;; start, so the loop repeats seamlessly.
;;
;; Finally the float buffer is converted to 16-bit PCM and wrapped in a
;; WAV header.

;;; Code:

(require 'cl-lib)

(defconst beatmacs-synth--2pi (* 2.0 float-pi))

;;;; Helpers

(defsubst beatmacs-synth--noise ()
  "White noise sample in -1.0..1.0."
  (- (/ (float (random 65536)) 32768.0) 1.0))

(defsubst beatmacs-synth--softclip (x)
  "Smooth saturation of X (a cheap tanh approximation).
Adds warm harmonics instead of hard digital clipping."
  (cond ((> x 3.0) 1.0)
        ((< x -3.0) -1.0)
        (t (let ((x2 (* x x)))
             (/ (* x (+ 27.0 x2)) (+ 27.0 (* 9.0 x2)))))))

(defun beatmacs-synth-midi-to-hz (note)
  "Convert MIDI NOTE number to frequency in Hz (A4 = 69 = 440 Hz)."
  (* 440.0 (expt 2.0 (/ (- note 69) 12.0))))

(defsubst beatmacs-synth--svf-f (cutoff sr)
  "Chamberlin state-variable filter coefficient for CUTOFF at SR.
The cutoff is clamped to keep the filter numerically stable."
  (* 2.0 (sin (/ (* float-pi (min cutoff (* sr 0.16))) sr))))

(defun beatmacs-synth--samples (seconds sr)
  "Number of samples for SECONDS at sample rate SR."
  (max 1 (round (* seconds sr))))

;;;; Drum voices

(cl-defun beatmacs-synth-kick (sr &key (pitch 52.0) (decay 0.45) (click 0.5))
  "Kick drum: a sine whose PITCH sweep gives the punch.
DECAY is the length in seconds, CLICK the amount of attack noise."
  (let* ((n (beatmacs-synth--samples decay sr))
         (v (make-vector n 0.0))
         (phase 0.0)
         (start (* pitch 3.0))
         (rate (/ 5.0 decay)))
    (dotimes (i n)
      (let* ((tm (/ (float i) sr))
             (f (+ pitch (* (- start pitch) (exp (/ tm -0.03)))))
             (amp (exp (* (- rate) tm)))
             (clk (* click 0.6 (beatmacs-synth--noise) (exp (/ tm -0.002)))))
        (setq phase (+ phase (/ (* beatmacs-synth--2pi f) sr)))
        (aset v i (beatmacs-synth--softclip
                   (* 1.4 (+ (* amp (sin phase)) clk))))))
    v))

(cl-defun beatmacs-synth-snare (sr &key (tone 185.0) (decay 0.22) (snappy 0.7))
  "Snare: a short pitched body (TONE Hz) plus high-passed noise.
SNAPPY sets how much of the noisy wire sound is mixed in."
  (let* ((n (beatmacs-synth--samples decay sr))
         (v (make-vector n 0.0))
         (hp 0.0) (prev 0.0) (a 0.75)
         (w1 (/ (* beatmacs-synth--2pi tone) sr))
         (w2 (/ (* beatmacs-synth--2pi tone 1.78) sr)))
    (dotimes (i n)
      (let* ((tm (/ (float i) sr))
             (body (* (exp (* -35.0 tm))
                      (+ (sin (* w1 i)) (* 0.5 (sin (* w2 i))))))
             (x (beatmacs-synth--noise)))
        (setq hp (* a (+ hp (- x prev)))
              prev x)
        (aset v i (* 1.3 (+ (* (- 1.0 snappy) body)
                            (* snappy hp (exp (/ (* -4.5 tm) decay))))))))
    v))

(cl-defun beatmacs-synth-clap (sr &key (decay 0.3))
  "Hand clap: three quick noise bursts plus a short reverb-ish tail."
  (let* ((n (beatmacs-synth--samples decay sr))
         (v (make-vector n 0.0))
         (f (beatmacs-synth--svf-f 1300.0 sr))
         (low 0.0) (band 0.0) (q 0.6))
    (dotimes (i n)
      (let* ((tm (/ (float i) sr))
             (env (cond ((< tm 0.01) (exp (/ tm -0.003)))
                        ((< tm 0.02) (exp (/ (- tm 0.01) -0.003)))
                        ((< tm 0.03) (exp (/ (- tm 0.02) -0.003)))
                        (t (* 0.8 (exp (/ (- tm 0.03) -0.07))))))
             (x (beatmacs-synth--noise))
             (high (- x low (* q band))))
        (setq band (+ band (* f high))
              low (+ low (* f band)))
        (aset v i (* 1.6 env band))))
    v))

(cl-defun beatmacs-synth-hat (sr &key (decay 0.04))
  "Hi-hat: noise through two high-pass stages, very short DECAY.
A long DECAY (0.2 s and up) gives an open hi-hat."
  (let* ((n (beatmacs-synth--samples (* decay 6.0) sr))
         (v (make-vector n 0.0))
         (h1 0.0) (p1 0.0) (h2 0.0) (p2 0.0) (a 0.55))
    (dotimes (i n)
      (let ((tm (/ (float i) sr))
            (x (beatmacs-synth--noise)))
        (setq h1 (* a (+ h1 (- x p1))) p1 x)
        (setq h2 (* a (+ h2 (- h1 p2))) p2 h1)
        (aset v i (* 1.2 h2 (exp (/ tm (- decay)))))))
    v))

;;;; Bass voices

(defsubst beatmacs-synth--gate (i n-on n-rel n-att)
  "Attack/sustain/release gain for sample I.
N-ATT attack samples, N-ON samples held, N-REL release samples."
  (cond ((< i n-att) (/ (float i) n-att))
        ((< i n-on) 1.0)
        (t (max 0.0 (- 1.0 (/ (float (- i n-on)) n-rel))))))

(cl-defun beatmacs-synth-808 (sr note dur &key (decay 0.8) (drive 1.8))
  "TR-808 style bass: a sine with a short pitch drop and long decay.
NOTE is a MIDI note, DUR the held time in seconds.  DRIVE saturates
the sine so that harmonics appear; this is what makes an 808 audible
on small (phone) speakers that cannot reproduce the fundamental."
  (let* ((f (beatmacs-synth-midi-to-hz note))
         (len (+ dur 0.05))
         (n (beatmacs-synth--samples len sr))
         (n-on (beatmacs-synth--samples dur sr))
         (n-rel (beatmacs-synth--samples 0.05 sr))
         (v (make-vector n 0.0))
         (phase 0.0)
         (norm (beatmacs-synth--softclip drive)))
    (dotimes (i n)
      (let* ((tm (/ (float i) sr))
             (fi (* f (+ 1.0 (* 0.6 (exp (/ tm -0.015))))))
             (amp (* (exp (/ tm (- decay)))
                     (beatmacs-synth--gate i n-on n-rel 20))))
        (setq phase (+ phase (/ (* beatmacs-synth--2pi fi) sr)))
        (aset v i (* amp (/ (beatmacs-synth--softclip (* drive (sin phase)))
                            norm)))))
    v))

(cl-defun beatmacs-synth-sub (sr note dur)
  "Clean sub bass: a pure sine.  Felt more than heard."
  (let* ((f (beatmacs-synth-midi-to-hz note))
         (n (beatmacs-synth--samples (+ dur 0.03) sr))
         (n-on (beatmacs-synth--samples dur sr))
         (n-rel (beatmacs-synth--samples 0.03 sr))
         (w (/ (* beatmacs-synth--2pi f) sr))
         (v (make-vector n 0.0)))
    (dotimes (i n)
      (aset v i (* 0.9 (sin (* w i)) (beatmacs-synth--gate i n-on n-rel 60))))
    v))

(cl-defun beatmacs-synth-reese (sr note dur &key (detune 15.0) (cutoff 700.0)
                                   (resonance 0.5))
  "Reese bass (classic Drum & Bass): two detuned saws, low-passed.
DETUNE is in cents; the two saws drift in and out of phase, giving
the moving, growling texture.  A sine sub underneath keeps the low end."
  (let* ((f (beatmacs-synth-midi-to-hz note))
         (r (expt 2.0 (/ detune 1200.0)))
         (d1 (/ (* f r) sr)) (d2 (/ (/ f r) sr))
         (ws (/ (* beatmacs-synth--2pi f) sr))
         (n (beatmacs-synth--samples (+ dur 0.04) sr))
         (n-on (beatmacs-synth--samples dur sr))
         (n-rel (beatmacs-synth--samples 0.04 sr))
         (ff (beatmacs-synth--svf-f cutoff sr))
         (q (max 0.1 (- 1.0 resonance)))
         (p1 0.0) (p2 0.25) (low 0.0) (band 0.0)
         (v (make-vector n 0.0)))
    (dotimes (i n)
      (setq p1 (+ p1 d1) p2 (+ p2 d2))
      (when (>= p1 1.0) (setq p1 (- p1 1.0)))
      (when (>= p2 1.0) (setq p2 (- p2 1.0)))
      (let* ((x (* 0.5 (+ (- (* 2.0 p1) 1.0) (- (* 2.0 p2) 1.0))))
             (high (- x low (* q band))))
        (setq band (+ band (* ff high))
              low (+ low (* ff band)))
        (aset v i (* (beatmacs-synth--gate i n-on n-rel 40)
                     (beatmacs-synth--softclip
                      (+ (* 0.8 low) (* 0.5 (sin (* ws i)))))))))
    v))

(cl-defun beatmacs-synth-wobble (sr note dur bpm &key (rate 2.0) (cutoff 1800.0)
                                    (resonance 0.6) (drive 2.0))
  "Dubstep wobble bass: saw + square through a low-pass filter whose
cutoff is swept by an LFO (low-frequency oscillator).
RATE is the LFO speed in cycles per beat at BPM, so the wobble is
locked to the tempo: 1 = quarter notes, 2 = eighths, 3 = eighth
triplets, 4 = sixteenths.  CUTOFF is the highest filter opening in Hz,
RESONANCE (0..0.95) emphasizes the cutoff (the \"vowel\" / \"yoy\"
sound), DRIVE adds distortion."
  (let* ((f (beatmacs-synth-midi-to-hz note))
         (dp (/ f sr))
         (ws (/ (* beatmacs-synth--2pi f) sr))
         (lfo-w (/ (* beatmacs-synth--2pi rate (/ bpm 60.0)) sr))
         (n (beatmacs-synth--samples (+ dur 0.04) sr))
         (n-on (beatmacs-synth--samples dur sr))
         (n-rel (beatmacs-synth--samples 0.04 sr))
         (q (max 0.08 (- 1.0 resonance)))
         (lo 90.0)
         (p 0.0) (low 0.0) (band 0.0)
         (norm (beatmacs-synth--softclip drive))
         (v (make-vector n 0.0)))
    (dotimes (i n)
      (setq p (+ p dp))
      (when (>= p 1.0) (setq p (- p 1.0)))
      (let* ((lfo (- 0.5 (* 0.5 (cos (* lfo-w i)))))
             (fc (+ lo (* (- cutoff lo) lfo lfo)))
             (ff (beatmacs-synth--svf-f fc sr))
             (x (+ (- (* 2.0 p) 1.0) (* 0.5 (if (< p 0.5) 1.0 -1.0))))
             (high (- x low (* q band))))
        (setq band (+ band (* ff high))
              low (+ low (* ff band)))
        (aset v i (* (beatmacs-synth--gate i n-on n-rel 40)
                     (+ (* 0.75 (/ (beatmacs-synth--softclip (* drive low)) norm))
                        (* 0.45 (sin (* ws i))))))))
    v))

;;;; Voice dispatch

(defun beatmacs-synth-voice (inst sr note dur bpm params)
  "Render one voice INST as a float vector.
SR sample rate, NOTE MIDI note (bass only), DUR held seconds,
BPM tempo, PARAMS a plist of instrument settings."
  (pcase inst
    ('kick (beatmacs-synth-kick sr :pitch (float (plist-get params :kick-pitch))
                                :decay (plist-get params :kick-decay)))
    ('snare (beatmacs-synth-snare sr))
    ('clap (beatmacs-synth-clap sr))
    ('hat (beatmacs-synth-hat sr :decay (plist-get params :hat-decay)))
    ('ohat (beatmacs-synth-hat sr :decay 0.22))
    ('808 (beatmacs-synth-808 sr note dur
                              :decay (plist-get params :decay)
                              :drive (plist-get params :drive)))
    ('sub (beatmacs-synth-sub sr note dur))
    ('reese (beatmacs-synth-reese sr note dur
                                  :detune (float (plist-get params :detune))
                                  :cutoff (float (plist-get params :cutoff))
                                  :resonance (plist-get params :resonance)))
    ('wobble (beatmacs-synth-wobble sr note dur bpm
                                    :rate (float (plist-get params :rate))
                                    :cutoff (float (plist-get params :cutoff))
                                    :resonance (plist-get params :resonance)
                                    :drive (plist-get params :drive)))
    (_ (error "beatmacs: unknown instrument %S" inst))))

;;;; Mixer

(defun beatmacs-synth-mix (events loop-seconds sr bpm &optional master)
  "Render EVENTS into a loop of LOOP-SECONDS at sample rate SR.
Each event is a plist with :time :inst :vel :gain and, for pitched
voices, :note :dur :params.  Identical voices are rendered once and
re-used.  Sound past the loop end wraps to the start (seamless loop).
MASTER is the output gain before limiting.  Returns a float vector."
  (random "beatmacs")                   ; deterministic noise
  (let* ((n (beatmacs-synth--samples loop-seconds sr))
         (out (make-vector n 0.0))
         (cache (make-hash-table :test #'equal)))
    (dolist (ev events)
      (let* ((inst (plist-get ev :inst))
             (note (plist-get ev :note))
             (dur (plist-get ev :dur))
             (params (plist-get ev :params))
             (key (list inst note (and dur (round (* dur 1000))) params))
             (voice (or (gethash key cache)
                        (puthash key (beatmacs-synth-voice
                                      inst sr note dur bpm params)
                                 cache)))
             (g (* (or (plist-get ev :vel) 1.0) (or (plist-get ev :gain) 1.0)))
             (start (mod (round (* (plist-get ev :time) sr)) n))
             (len (length voice)))
        (dotimes (j len)
          (let ((idx (% (+ start j) n)))
            (aset out idx (+ (aref out idx) (* g (aref voice j))))))))
    ;; Master: normalize the sum to peak 1.0, apply MASTER as drive into
    ;; a gentle saturator (glues the mix, louder), then set the final
    ;; peak to -1 dBFS.  So MASTER changes character, not just volume.
    (let ((m (or master 1.0)) (peak 0.0))
      (dotimes (i n) (setq peak (max peak (abs (aref out i)))))
      (when (> peak 0.0) (setq m (/ m peak)))
      (setq peak 0.0)
      (dotimes (i n)
        (let ((y (beatmacs-synth--softclip (* m (aref out i)))))
          (aset out i y)
          (setq peak (max peak (abs y)))))
      (when (> peak 0.0)
        (let ((k (/ 0.89 peak)))
          (dotimes (i n) (aset out i (* k (aref out i)))))))
    out))

;;;; WAV encoding

(defun beatmacs-synth-pcm16 (samples)
  "Convert float vector SAMPLES to a unibyte string of 16-bit LE PCM."
  (let* ((n (length samples))
         (s (make-string (* 2 n) 0)))
    (dotimes (i n)
      (let* ((x (max -1.0 (min 1.0 (aref samples i))))
             (v (logand (round (* x 32767.0)) #xFFFF)))
        (aset s (* 2 i) (logand v #xFF))
        (aset s (1+ (* 2 i)) (ash v -8))))
    s))

(defun beatmacs-synth--le (value bytes)
  "VALUE as a little-endian unibyte string of BYTES length."
  (let ((s (make-string bytes 0)))
    (dotimes (i bytes)
      (aset s i (logand (ash value (* -8 i)) #xFF)))
    s))

(defun beatmacs-synth-write-wav (file sr pcm &optional repeat)
  "Write mono 16-bit PCM string PCM at SR into FILE as WAV.
REPEAT (default 1) writes the PCM that many times back-to-back,
which turns a loop into a longer track cheaply."
  (let* ((repeat (or repeat 1))
         (data-len (* (length pcm) repeat)))
    (make-directory (file-name-directory (expand-file-name file)) t)
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert "RIFF" (beatmacs-synth--le (+ 36 data-len) 4) "WAVE"
              "fmt " (beatmacs-synth--le 16 4)
              (beatmacs-synth--le 1 2)            ; PCM
              (beatmacs-synth--le 1 2)            ; mono
              (beatmacs-synth--le sr 4)
              (beatmacs-synth--le (* sr 2) 4)     ; byte rate
              (beatmacs-synth--le 2 2)            ; block align
              (beatmacs-synth--le 16 2)           ; bits/sample
              "data" (beatmacs-synth--le data-len 4))
      (dotimes (_ repeat) (insert pcm))
      (let ((coding-system-for-write 'binary))
        (write-region nil nil file nil 'silent)))
    file))

(provide 'beatmacs-synth)
;;; beatmacs-synth.el ends here
