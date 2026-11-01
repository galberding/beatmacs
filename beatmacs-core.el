;;; beatmacs-core.el --- Track model, registries, sequencing, backends -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Georg

;; This file is not part of GNU Emacs.

;;; Commentary:

;; The data model and the extension points of beatmacs.
;;
;; A TRACK has global settings (genre, pattern variant, tempo, swing,
;; length, key) and a list of LAYERS.  Each layer has a TYPE (drums,
;; bass, ... later melody), a volume, a mute flag and a params plist.
;;
;; Extension points (all plain registries, see the docstrings):
;;
;;   `beatmacs-define-genre'       genres and their pattern variants
;;   `beatmacs-define-layer-type'  new kinds of layers (e.g. melody)
;;   `beatmacs-define-backend'     new sound engines (MIDI, SoX, ...)
;;
;; Sequencing turns a track into EVENTS: plists like
;;   (:time 1.25 :inst kick :vel 0.75 :gain 0.9)
;;   (:time 0.0 :inst wobble :note 29 :dur 0.86 :params (...) ...)
;; A backend turns events into sound.  Because the events are
;; backend-neutral, a MIDI backend can later consume the same list.

;;; Code:

(require 'cl-lib)
(require 'beatmacs-synth)

(defgroup beatmacs nil
  "Beat generator for rapping, in Emacs."
  :group 'multimedia
  :prefix "beatmacs-")

(defcustom beatmacs-sample-rate 22050
  "Sample rate used for rendering.
22050 Hz renders twice as fast as 44100 and is plenty for drums and
bass.  Raise it for brighter hi-hats in exports."
  :type '(choice (const 11025) (const 22050) (const 32000) (const 44100)))

(defcustom beatmacs-backend 'elisp
  "Sound engine used to render tracks.  See `beatmacs-define-backend'."
  :type 'symbol)

(defcustom beatmacs-directory (locate-user-emacs-file "beatmacs/")
  "Base directory for the render cache and presets."
  :type 'directory)

;;;; Model

(cl-defstruct (beatmacs-layer (:constructor beatmacs-layer--make))
  type name (volume 0.8) mute params)

(cl-defstruct (beatmacs-track (:constructor beatmacs-track--make))
  (name "untitled") genre variant (bpm 120) (swing 0) (bars 4) (key 5)
  layers (master 1.3))

(defconst beatmacs-note-names
  ["C" "C#" "D" "D#" "E" "F" "F#" "G" "G#" "A" "A#" "B"])

(defconst beatmacs-minor-scale [0 2 3 5 7 8 10]
  "Natural minor scale as semitone offsets.  Most rap, dubstep and
DnB lives in minor keys: it sounds darker and more tense.")

;;;; Registries

(defvar beatmacs--genres nil "Alist of (ID . PLIST), in definition order.")
(defvar beatmacs--layer-types nil "Alist of (TYPE . PLIST).")
(defvar beatmacs--backends nil "Alist of (ID . PLIST).")

(defun beatmacs--register (var id plist)
  "Add or replace ID with PLIST in registry VAR, keeping order."
  (let ((cell (assq id (symbol-value var))))
    (if cell (setcdr cell plist)
      (set var (append (symbol-value var) (list (cons id plist)))))
    id))

(defun beatmacs-define-genre (id &rest plist)
  "Define genre ID.
PLIST keys:
  :label      display name
  :bpm        default tempo
  :swing      default swing (0-100)
  :key        default key (0 = C ... 11 = B)
  :bass-type  default bass instrument (808 sub reese wobble)
  :notes      learning notes (string)
  :rap-tips   tips for rapping over it (string)
  :variants   list of (VID . VPLIST); VPLIST may override any key
              above and must contain
                :label
                :drums  plist of lanes (:kick :snare :clap :hat :ohat)
                        each a step string, 16 chars per bar:
                        X accent, x normal, o ghost, r 2-hit ratchet,
                        R 3-hit ratchet, anything else = rest
                :bass   list of (STEP LEN SEMITONES [LFO-RATE])
                :progression  scale degrees per bar, e.g. (0 5 3 4)"
  (beatmacs--register 'beatmacs--genres id plist))

(defun beatmacs-define-layer-type (type &rest plist)
  "Define layer TYPE.
PLIST keys:
  :label     display name
  :defaults  default params plist
  :volume    default volume
  :events    function (TRACK LAYER) -> list of events
  :preview   function (TRACK LAYER WIDTH) -> short string for the
             track list
  :columns   function (TRACK LAYER) -> list of (TITLE OPTION-ID...)
             shown on the Sound page when this layer is selected"
  (beatmacs--register 'beatmacs--layer-types type plist))

(defun beatmacs-define-backend (id &rest plist)
  "Define a sound backend ID.
PLIST keys:
  :label        display name
  :available-p  function -> non-nil if usable here (optional)
  :render       function (TRACK FILE SAMPLE-RATE &optional SECONDS)
                that writes a WAV to FILE and returns FILE.  With
                SECONDS, the loop is repeated to last at least that long."
  (beatmacs--register 'beatmacs--backends id plist))

(defun beatmacs-genre (id) (cdr (assq id beatmacs--genres)))
(defun beatmacs-layer-spec (type)
  "Registry entry of layer TYPE."
  (cdr (assq type beatmacs--layer-types)))
(defun beatmacs-genre-ids () (mapcar #'car beatmacs--genres))

(defun beatmacs-variant (track)
  "The variant plist of TRACK."
  (cdr (assq (beatmacs-track-variant track)
             (plist-get (beatmacs-genre (beatmacs-track-genre track))
                        :variants))))

(defun beatmacs-style-get (track key)
  "KEY from TRACK's variant, falling back to its genre."
  (let ((v (beatmacs-variant track)))
    (if (plist-member v key) (plist-get v key)
      (plist-get (beatmacs-genre (beatmacs-track-genre track)) key))))

;;;; Construction

(defun beatmacs-make-layer (type)
  "New layer of TYPE with its defaults."
  (let ((spec (beatmacs-layer-spec type)))
    (beatmacs-layer--make :type type
                          :name (plist-get spec :label)
                          :volume (or (plist-get spec :volume) 0.8)
                          :params (copy-tree (plist-get spec :defaults)))))

(defun beatmacs-apply-style (track genre &optional variant)
  "Switch TRACK to GENRE / VARIANT and load that style's defaults."
  (let* ((g (beatmacs-genre genre))
         (variant (or variant (car (car (plist-get g :variants))))))
    (setf (beatmacs-track-genre track) genre
          (beatmacs-track-variant track) variant
          (beatmacs-track-bpm track) (beatmacs-style-get track :bpm)
          (beatmacs-track-swing track) (or (beatmacs-style-get track :swing) 0))
    (when (beatmacs-style-get track :key)
      (setf (beatmacs-track-key track) (beatmacs-style-get track :key)))
    track))

(defun beatmacs-make-track (&optional genre)
  "New track in GENRE (default: first defined) with drums and bass."
  (let ((tr (beatmacs-track--make
             :layers (list (beatmacs-make-layer 'drums)
                           (beatmacs-make-layer 'bass)))))
    (beatmacs-apply-style tr (or genre (car (beatmacs-genre-ids))))))

(defun beatmacs-param (layer key)
  "Value of KEY in LAYER's params."
  (plist-get (beatmacs-layer-params layer) key))

(defun beatmacs-set-param (layer key value)
  "Set KEY to VALUE in LAYER's params."
  (setf (beatmacs-layer-params layer)
        (plist-put (beatmacs-layer-params layer) key value)))

;;;; Timing

(defun beatmacs-step-seconds (track)
  "Length of one 16th-note step of TRACK in seconds."
  (/ 60.0 (beatmacs-track-bpm track) 4.0))

(defun beatmacs-loop-seconds (track)
  "Length of TRACK's loop in seconds."
  (* (beatmacs-track-bars track) 16 (beatmacs-step-seconds track)))

(defun beatmacs-step-time (track step)
  "Start time in seconds of 16th STEP, including swing.
Swing delays every second 16th note.  At 100 the delay reaches a
full triplet feel; 20-40 is the typical laid-back hip-hop groove."
  (let ((sd (beatmacs-step-seconds track)))
    (+ (* step sd)
       (if (cl-oddp step)
           (* (/ (beatmacs-track-swing track) 100.0) (/ sd 3.0))
         0.0))))

(defun beatmacs-root-note (track octave degree)
  "MIDI note for scale DEGREE of TRACK's key in OCTAVE."
  (+ (* 12 (1+ octave))
     (beatmacs-track-key track)
     (* 12 (floor degree 7))
     (aref beatmacs-minor-scale (mod degree 7))))

;;;; Step strings

(defun beatmacs-step-hits (char)
  "Velocity and hit count for step CHAR, or nil for a rest."
  (pcase char
    (?X '(1.0 . 1)) (?x '(0.75 . 1)) (?o '(0.4 . 1))
    (?r '(0.6 . 2)) (?R '(0.6 . 3))
    (_ nil)))

(defun beatmacs-lane-events (track pattern inst gain)
  "Events for INST playing step string PATTERN across TRACK's bars."
  (let* ((steps (* 16 (beatmacs-track-bars track)))
         (len (length pattern))
         (sd (beatmacs-step-seconds track))
         events)
    (when (> len 0)
      (dotimes (s steps)
        (let ((hit (beatmacs-step-hits (aref pattern (mod s len)))))
          (when hit
            (let ((t0 (beatmacs-step-time track s))
                  (k (cdr hit)))
              (dotimes (h k)
                (push (list :time (+ t0 (* h (/ sd k)))
                            :inst inst :vel (car hit) :gain gain)
                      events)))))))
    (nreverse events)))

;;;; Track -> events

(defun beatmacs-track-events (track)
  "All events of TRACK's unmuted layers."
  (cl-loop for layer in (beatmacs-track-layers track)
           unless (beatmacs-layer-mute layer)
           append (funcall (plist-get (beatmacs-layer-spec
                                       (beatmacs-layer-type layer))
                                      :events)
                           track layer)))

;;;; Backends

(defun beatmacs-render (track file &optional seconds sample-rate)
  "Render TRACK into FILE with the current backend.
SECONDS: minimum length (loop repeated); SAMPLE-RATE overrides
`beatmacs-sample-rate'."
  (let ((b (cdr (assq beatmacs-backend beatmacs--backends))))
    (unless b (user-error "beatmacs: unknown backend %S" beatmacs-backend))
    (funcall (plist-get b :render) track file
             (or sample-rate beatmacs-sample-rate) seconds)))

(defun beatmacs--elisp-render (track file sr &optional seconds)
  "Pure Elisp backend: render TRACK to FILE at SR (see `beatmacs-render')."
  (let* ((loop (beatmacs-loop-seconds track))
         (samples (beatmacs-synth-mix (beatmacs-track-events track)
                                      loop sr (beatmacs-track-bpm track)
                                      (beatmacs-track-master track)))
         (repeat (if seconds (max 1 (ceiling (/ seconds loop))) 1)))
    (beatmacs-synth-write-wav file sr (beatmacs-synth-pcm16 samples) repeat)))

(beatmacs-define-backend 'elisp
  :label "Pure Elisp"
  :render #'beatmacs--elisp-render)

;;;; Serialization (presets)

(defun beatmacs-track-to-plist (track)
  "Readable plist describing TRACK."
  (list :format 1
        :name (beatmacs-track-name track)
        :genre (beatmacs-track-genre track)
        :variant (beatmacs-track-variant track)
        :bpm (beatmacs-track-bpm track)
        :swing (beatmacs-track-swing track)
        :bars (beatmacs-track-bars track)
        :key (beatmacs-track-key track)
        :master (beatmacs-track-master track)
        :layers (mapcar (lambda (l)
                          (list :type (beatmacs-layer-type l)
                                :name (beatmacs-layer-name l)
                                :volume (beatmacs-layer-volume l)
                                :mute (beatmacs-layer-mute l)
                                :params (beatmacs-layer-params l)))
                        (beatmacs-track-layers track))))

(defun beatmacs-track-from-plist (pl)
  "Rebuild a track from plist PL (see `beatmacs-track-to-plist')."
  (beatmacs-track--make
   :name (plist-get pl :name) :genre (plist-get pl :genre)
   :variant (plist-get pl :variant) :bpm (plist-get pl :bpm)
   :swing (plist-get pl :swing) :bars (plist-get pl :bars)
   :key (plist-get pl :key) :master (or (plist-get pl :master) 1.3)
   :layers (mapcar (lambda (l)
                     (let ((layer (beatmacs-make-layer (plist-get l :type))))
                       ;; Merge saved params over defaults so presets
                       ;; survive new params being added later.
                       (cl-loop for (k v) on (plist-get l :params) by #'cddr
                                do (beatmacs-set-param layer k v))
                       (setf (beatmacs-layer-name layer) (plist-get l :name)
                             (beatmacs-layer-volume layer) (plist-get l :volume)
                             (beatmacs-layer-mute layer) (plist-get l :mute))
                       layer))
                   (plist-get pl :layers))))

(provide 'beatmacs-core)
;;; beatmacs-core.el ends here
