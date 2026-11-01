;;; beatmacs-styles.el --- Layer types, genres and learning notes -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Georg

;; This file is not part of GNU Emacs.

;;; Commentary:

;; The musical content: the drums and bass layer types and the three
;; MVP genres (Dubstep, Drum & Bass, Rap/R&B), each with a few pattern
;; variants and learning notes.
;;
;; Patterns are plain step strings, 16 characters per bar (one per 16th
;; note), so they are easy to read and to change:
;;
;;   beat:  1 . . . 2 . . . 3 . . . 4 . . .
;;   kick  "X.........X....."     X accent  x normal  o ghost
;;   snare "....X.......X..."     r = 2 fast hits, R = 3 fast hits
;;
;; To add your own variant, evaluate something like
;;
;;   (beatmacs-add-variant 'rap 'my-groove
;;     :label "My groove"
;;     :drums '(:kick "X..x..X...X....." :snare "....X.......X...")
;;     :bass '((0 4 0) (10 6 0)) :progression '(0 3))

;;; Code:

(require 'cl-lib)
(require 'beatmacs-core)

(defun beatmacs-add-variant (genre id &rest plist)
  "Add (or replace) pattern variant ID with PLIST to GENRE.
See `beatmacs-define-genre' for the keys."
  (let* ((g (beatmacs-genre genre))
         (vs (plist-get g :variants))
         (cell (assq id vs)))
    (if cell (setcdr cell plist)
      (setq vs (append vs (list (cons id plist)))))
    (apply #'beatmacs-define-genre genre
           (plist-put (copy-sequence g) :variants vs))))

;;;; Drums layer

(defconst beatmacs--hat-overrides
  '((8ths . "x.x.x.x.x.x.x.x.")
    (16ths . "xoxoxoxoxoxoxoxo")
    (rolls . "x.x.x.x.x.xrx.Rx"))
  "Hi-hat patterns that replace the variant's hats.")

(defun beatmacs--drum-events (track layer)
  "Events for drums LAYER of TRACK."
  (let* ((lanes (beatmacs-style-get track :drums))
         (g (beatmacs-layer-volume layer))
         (hats (beatmacs-param layer :hats))
         (snare (let ((s (beatmacs-param layer :snare)))
                  (if (eq s 'auto)
                      (or (beatmacs-style-get track :snare-sound) 'snare)
                    s)))
         (kick-params (list :kick-pitch (beatmacs-param layer :kick-pitch)
                            :kick-decay (beatmacs-param layer :kick-decay)))
         (hat-params (list :hat-decay (beatmacs-param layer :hat-decay)))
         (hat-pattern (pcase hats
                        ('pattern (plist-get lanes :hat))
                        ('off nil)
                        (_ (alist-get hats beatmacs--hat-overrides))))
         (with (lambda (evs params)
                 (dolist (e evs evs) (nconc e (list :params params))))))
    (append
     (funcall with (beatmacs-lane-events track (or (plist-get lanes :kick) "")
                                         'kick g)
              kick-params)
     (when (memq snare '(snare both))
       (beatmacs-lane-events track (or (plist-get lanes :snare) "") 'snare g))
     (when (memq snare '(clap both))
       (beatmacs-lane-events track (or (plist-get lanes :snare) "") 'clap
                             (* g (if (eq snare 'both) 0.6 1.0))))
     (beatmacs-lane-events track (or (plist-get lanes :clap) "") 'clap g)
     (funcall with (beatmacs-lane-events track (or hat-pattern "") 'hat (* g 0.8))
              hat-params)
     (unless (eq hats 'off)
       (beatmacs-lane-events track (or (plist-get lanes :ohat) "") 'ohat
                             (* g 0.5))))))

(defun beatmacs--drum-preview (track _layer width)
  "One bar of drums as WIDTH characters: K kick, S snare, o open, h hat."
  (let* ((lanes (beatmacs-style-get track :drums))
         (at (lambda (lane i)
               (let ((s (plist-get lanes lane)))
                 (and s (> (length s) 0)
                      (beatmacs-step-hits (aref s (mod i (length s)))))))))
    (truncate-string-to-width
     (apply #'string
            (cl-loop for i below 16
                     collect (cond ((funcall at :kick i) ?K)
                                   ((or (funcall at :snare i)
                                        (funcall at :clap i))
                                    ?S)
                                   ((funcall at :ohat i) ?o)
                                   ((funcall at :hat i) ?h)
                                   (t ?·))))
     width)))

(beatmacs-define-layer-type 'drums
  :label "Drums"
  :volume 0.9
  :defaults '(:kick-pitch 52 :kick-decay 0.45 :snare auto
              :hats pattern :hat-decay 0.04)
  :events #'beatmacs--drum-events
  :preview #'beatmacs--drum-preview
  :columns (lambda (_track _layer)
             '(("Kick" kick-pitch kick-decay)
               ("Snare & hats" snare-sound hats hat-decay))))

;;;; Bass layer

(defconst beatmacs-bass-default-cutoff '((reese . 700) (wobble . 1800))
  "Filter cutoff (Hz) used when the bass cutoff is set to auto.")

(defun beatmacs-bass-type (track layer)
  "Effective bass instrument of LAYER in TRACK (resolves `auto')."
  (let ((ty (beatmacs-param layer :type)))
    (if (eq ty 'auto) (or (beatmacs-style-get track :bass-type) 'sub) ty)))

(defun beatmacs-bass-cutoff (track layer)
  "Effective filter cutoff of LAYER in TRACK."
  (or (beatmacs-param layer :cutoff)
      (alist-get (beatmacs-bass-type track layer) beatmacs-bass-default-cutoff
                 1000)))

(defun beatmacs--bass-events (track layer)
  "Events for bass LAYER of TRACK."
  (let* ((type (beatmacs-bass-type track layer))
         (octave (beatmacs-param layer :octave))
         (prog (or (beatmacs-style-get track :progression) '(0)))
         (notes (beatmacs-style-get track :bass))
         (sd (beatmacs-step-seconds track))
         (rate (beatmacs-param layer :rate))
         (g (beatmacs-layer-volume layer))
         events)
    (dotimes (bar (beatmacs-track-bars track))
      (let ((root (beatmacs-root-note track octave
                                      (nth (mod bar (length prog)) prog))))
        (pcase-dolist (`(,step ,len ,semi ,lfo) notes)
          (push (list :time (beatmacs-step-time track (+ (* bar 16) step))
                      :inst type
                      :note (+ root semi)
                      :dur (* len sd 0.95)
                      :vel 1.0 :gain g
                      :params
                      (pcase type
                        ('808 (list :decay (beatmacs-param layer :decay)
                                    :drive (beatmacs-param layer :drive)))
                        ('reese (list :detune (beatmacs-param layer :detune)
                                      :cutoff (beatmacs-bass-cutoff track layer)
                                      :resonance (beatmacs-param layer :resonance)))
                        ('wobble (list :rate (if (eq rate 'pattern) (or lfo 2) rate)
                                       :cutoff (beatmacs-bass-cutoff track layer)
                                       :resonance (beatmacs-param layer :resonance)
                                       :drive (beatmacs-param layer :drive)))
                        (_ nil)))
                events))))
    (nreverse events)))

(defun beatmacs--bass-preview (track _layer width)
  "One bar of bass as WIDTH characters: ■ note start, ─ held, · rest."
  (let ((cells (make-vector 16 ?·)))
    (pcase-dolist (`(,step ,len . ,_) (beatmacs-style-get track :bass))
      (dotimes (k len)
        (when (< (+ step k) 16)
          (aset cells (+ step k) (if (= k 0) ?■ ?─)))))
    (truncate-string-to-width (concat cells) width)))

(beatmacs-define-layer-type 'bass
  :label "Bass"
  :volume 0.65
  :defaults '(:type auto :octave 1 :decay 0.8 :drive 1.8 :cutoff nil
              :resonance 0.6 :rate pattern :detune 15)
  :events #'beatmacs--bass-events
  :preview #'beatmacs--bass-preview
  :columns (lambda (track layer)
             (pcase (beatmacs-bass-type track layer)
               ('808 '(("Bass" bass-type octave) ("Tone" decay drive)))
               ('sub '(("Bass" bass-type octave) ("Tone")))
               ('reese '(("Bass" bass-type octave detune)
                         ("Filter" cutoff resonance)))
               ('wobble '(("Bass" bass-type octave drive)
                          ("Filter" cutoff resonance wobble-rate))))))

;;;; Genres

(beatmacs-define-genre 'dubstep
  :label "Dubstep"
  :bpm 140 :swing 0 :key 5 :bass-type 'wobble
  :notes "\
Dubstep came out of South London around 2000-2005, from UK garage
(2-step) and Jamaican dub reggae.  It sits at 138-142 BPM, but it is
played HALF-TIME: the snare hits only once per bar, on beat 3.  So the
groove feels like 70 BPM - slow and heavy - while the hi-hats keep the
140 BPM energy on top.

The signature sound is the WOBBLE BASS: a buzzy saw wave sent through
a low-pass filter (it lets only low frequencies through).  An LFO
(low-frequency oscillator) opens and closes that filter in time with
the beat - \"wub wub wub\".  Changing the LFO speed from note to note
(quarters -> eighths -> sixteenths) is what makes the bass \"talk\".
Resonance makes the sweep sound vocal (\"yoy\"); drive adds grit."
  :rap-tips "\
- Feel the half-time pulse: count \"1 2 3 4\" at ~70 BPM (one count
  every four hi-hat ticks).  The snare then falls on YOUR 2 and 4, just like a
  hip-hop beat - land important words there.
- Grime / UK MCs ride the full 140 BPM with fast 16th-note flows:
  short phrases, lots of repetition, call-and-response.
- Leave room when the bass is busiest - rap in the gaps."
  :variants
  '((halftime
     :label "Half-time"
     :notes "The classic: kick on 1, snare on 3, the wobble changes speed every few steps."
     :drums (:kick "X.........x....." :snare "........X......."
             :hat "x.o.x.o.x.o.x.o." :ohat "..............x.")
     :bass ((0 6 0 2) (6 2 0 4) (8 4 0 3) (12 4 7 1))
     :progression (0 0 5 4))
    (riddim
     :label "Riddim"
     :notes "Minimal and bouncy: one note, short repeated wobbles, lots of silence. The gaps ARE the groove."
     :drums (:kick "X.....x...X....." :snare "........X......."
             :hat "....x.......x...")
     :bass ((0 2 0 4) (3 2 0 4) (6 2 0 4) (8 8 0 2))
     :progression (0))
    (brostep
     :label "Brostep"
     :notes "Aggressive US-style: octave jumps, very fast LFO rates (8 = 32nd notes). Try high resonance and drive."
     :drums (:kick "X.......x.X....." :snare "........X.....o."
             :clap "........X......." :hat "x.x.x.x.x.x.x.x.")
     :bass ((0 3 0 4) (3 3 12 3) (6 2 0 4) (8 4 0 2) (12 2 -2 4) (14 2 0 8))
     :progression (0 0 5 6))))

(beatmacs-define-genre 'dnb
  :label "Drum & Bass"
  :bpm 174 :swing 0 :key 5 :bass-type 'reese
  :notes "\
Drum & Bass grew out of UK jungle and breakbeat hardcore in the early
1990s.  It runs fast: 170-176 BPM, 174 is the standard today.  The
drums come from chopped-up funk breakbeats - most famously the
\"Amen break\".

The basic groove is the TWO-STEP: kick on beat 1 and on the \"and\"
of beat 3, snare on beats 2 and 4.  Small quiet GHOST snares between
the main hits make it roll.

Bass is either a deep clean SUB (felt more than heard) or a REESE: two
saw waves slightly out of tune with each other, so they drift in and
out of phase and growl.  Detune controls how fast that movement is.

Fun fact: half of 174 is 87 BPM - a typical hip-hop tempo.  That's
why rapping over DnB works so well."
  :rap-tips "\
- Most MCs ride the HALF-TIME pocket: rap as if it were an 87 BPM
  hip-hop beat.  Every SECOND snare is then your backbeat ('2' and '4').
- UK DnB MCs \"host\" with double-time: short repeated 16th-note
  phrases, hype lines, call-and-response with the crowd.
- Use the fills and drops: go quiet before the bass comes back."
  :variants
  '((two-step
     :label "Two-step"
     :notes "Kick on 1 and the 'and' of 3, snare on 2 and 4, ghost snare before beat 3. The DnB backbone."
     :drums (:kick "X.........X....." :snare "....X..o....X..."
             :hat "x.x.x.x.x.x.x.x." :ohat "..............x.")
     :bass ((0 10 0) (10 6 0))
     :progression (0 0 5 6))
    (rolling
     :label "Rolling"
     :bass-type sub
     :notes "Driving 16th hats and a busy rolling sub line - 'rollers' are made for long DJ mixes."
     :drums (:kick "X.........X..x.." :snare "....X.......X..o"
             :hat "xoxoxoxoxoxoxoxo")
     :bass ((0 3 0) (3 3 0) (6 4 0) (10 3 7) (13 3 0))
     :progression (0 0 3 5))
    (jungle
     :label "Jungle"
     :bpm 165 :swing 15 :bass-type sub
     :notes "Breakbeat chops in the style of the Amen break, with long reggae-style sub notes underneath."
     :drums (:kick "X.X.......XX...." :snare "....X..o.o..X..o"
             :hat "x.x.x.x.x.x.x.x." :ohat "......x.......x.")
     :bass ((0 16 0))
     :progression (0 5 3 4))))

(beatmacs-define-genre 'rap
  :label "Rap / R&B"
  :bpm 90 :swing 30 :key 5 :bass-type 'sub
  :notes "\
Hip-hop beats are built on a simple backbone: KICK on beat 1 (and
some off-beats), SNARE or CLAP on beats 2 and 4, hi-hats in eighths.

- Boom bap (85-95 BPM): the 90s New York sound, punchy kick and snare.
- Trap (130-150 BPM, counted half-time): the 808 bass doubles as the
  kick, snare/clap on beat 3, and fast hi-hat ROLLS (ratchets).
- R&B (60-80 BPM): slow, smooth, spacious.

SWING delays every second 16th note.  It's what makes a beat feel
laid-back and human instead of robotic - the MPC / J Dilla groove.
Try 0 vs 30 vs 50 on the boom-bap pattern and listen.

The 808 is a sine-wave bass with a long decay.  Phone speakers can't
play its low fundamental, so beatmacs adds DRIVE (saturation): extra
harmonics that your ear still hears as the bass note."
  :rap-tips "\
- Count 1-2-3-4 per bar.  One bar is roughly one line; a classic verse
  is 16 bars, a hook 8.
- Rhymes usually land on or near the SNARE (beats 2 and 4).
- Start ON beat 1 first; later try starting on the 'and' or just
  before the bar - that's where flow gets interesting.
- Lower BPM leaves space for more syllables (double-time); higher BPM
  wants fewer, punchier words."
  :variants
  '((boombap
     :label "Boom bap"
     :notes "Punchy kick-snare with swing. Kick pattern 'X......x.xX' is a classic head-nod."
     :drums (:kick "X......x.xX....." :snare "....X.......X..."
             :hat "x.x.x.x.x.x.x.x." :ohat "..............o.")
     :bass ((0 3 0) (7 2 0) (9 2 0) (10 4 -2))
     :progression (0 5 3 4))
    (trap
     :label "Trap"
     :bpm 140 :swing 0 :bass-type 808 :snare-sound clap
     :notes "Half-time clap on beat 3, 808s following the kick, hi-hat rolls (r/R = ratchets)."
     :drums (:kick "X......X..X....." :snare "........X......."
             :hat "x.x.x.x.x.xrx.Rx")
     :bass ((0 7 0) (7 3 0) (10 6 -2))
     :progression (0 0 5 6))
    (rnb
     :label "R&B slow"
     :bpm 72 :swing 20 :snare-sound both
     :notes "Slow and spacious with a layered snare+clap; the bass walks root - minor third - fifth."
     :drums (:kick "X.......x.x....." :snare "....X.......X..."
             :hat "x.x.x.x.x.xxx.x.")
     :bass ((0 6 0) (6 2 3) (8 8 7))
     :progression (0 5 2 6))))

;;;; General primer

(defconst beatmacs-primer "\
HOW A BEAT IS BUILT

TEMPO (BPM) - beats per minute.  You nod your head on the beats.

BARS & STEPS - 4 beats make a bar.  Each beat is split into four 16th
notes, so one bar = 16 STEPS.  The track list above shows one bar:
  K kick   S snare/clap   h hi-hat   o open hat   · rest
  ■ bass note starts   ─ bass note held

THE THREE DRUM ROLES
  Kick  - low thump, the 'downbeat', mostly on beat 1
  Snare - the crack on 2 and 4 (or only 3 in half-time styles)
  Hats  - the ticking that sets the energy and speed

BASS & KEY - The bass plays the root notes of a short chord
progression in a minor key.  Change KEY to fit your voice: if a hook
feels too high or low to sing, move the key up or down.

SWING - delays every second 16th; 0 = robotic, 20-40 = laid back.

HALF-TIME - snare once per bar instead of twice.  A 140 BPM beat then
feels like 70.  Dubstep and trap work this way.

LAYERS - Drums and Bass are separate layers; select one in the track
list to edit it on the Sound page, mute or level it on the Mix page.
More layer types (melody, chords) can be added with
`beatmacs-define-layer-type'.")

(provide 'beatmacs-styles)
;;; beatmacs-styles.el ends here
