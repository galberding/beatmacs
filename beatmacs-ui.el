;;; beatmacs-ui.el --- Touch- and keyboard-friendly control panel -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Georg

;; This file is not part of GNU Emacs.

;;; Commentary:

;; The *beatmacs* buffer, laid out like this (phone portrait):
;;
;;    ♪ Track header + [▶ Play] [↻ Render] [?]
;;    ─────────────────────────────
;;    ▸ Drums  K·h·S·h·K·h·S·h·     <- track layers, tap to select
;;      Bass   ■─────────■─────
;;      [+ Layer]
;;    ─────────────────────────────
;;     Option 1      │  Option 2      <- columns of the current page
;;     [ Val 1     ] │  [−]  174  [+]
;;     [ Val 2     ] │
;;                 ...
;;    [ P1 Beat ][ P2 Sound ][ P3 Mix ]   <- page tabs (bottom)
;;               [ Config ]
;;
;; Everything is an ordinary Emacs button, so it works with a mouse,
;; with taps on Android, and with the keyboard (TAB / S-TAB to move,
;; RET to press, + / - on any stepper, C-u for bigger steps).
;;
;; Extension: options are defined with `beatmacs-define-option' and
;; arranged into pages with `beatmacs-define-page'.  A page's columns
;; can be a function of the track and selected layer, which is how the
;; Sound page shows different controls for drums and bass.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'button)
(require 'beatmacs-core)
(require 'beatmacs-styles)
(require 'beatmacs-player)

;;;; Customization

(defcustom beatmacs-line-spacing 0.35
  "Extra line spacing in the panel; bigger = easier to tap."
  :type 'number :group 'beatmacs)

(defcustom beatmacs-export-seconds 120
  "Length of exported tracks in seconds (rounded up to whole loops)."
  :type 'integer :group 'beatmacs)

(defcustom beatmacs-export-sample-rate 44100
  "Sample rate for exports."
  :type 'integer :group 'beatmacs)

(defcustom beatmacs-export-directory
  (if (eq system-type 'android) "/sdcard/Music/beatmacs/" "~/Music/beatmacs/")
  "Where exported WAV files go."
  :type 'directory :group 'beatmacs)

(defcustom beatmacs-preset-directory
  (expand-file-name "presets/" beatmacs-directory)
  "Where presets are saved."
  :type 'directory :group 'beatmacs)

(defcustom beatmacs-auto-render t
  "Re-render and restart the loop automatically after a change
while playing."
  :type 'boolean :group 'beatmacs)

(defface beatmacs-button
  '((((type graphic)) :box (:line-width -1 :style released-button))
    (t :inherit button :underline nil))
  "Face for panel buttons.")

(defface beatmacs-selected
  '((t :inherit (bold beatmacs-button) :inverse-video t))
  "Face for the selected value / page / layer.")

(defface beatmacs-title '((t :inherit (bold font-lock-keyword-face)))
  "Face for titles.")

(defface beatmacs-dim '((t :inherit shadow))
  "Face for secondary text.")

;;;; State

(defvar-local beatmacs--track nil "The track edited in this buffer.")
(defvar-local beatmacs--page 'beat "Current page id.")
(defvar-local beatmacs--layer 0 "Index of the selected layer.")
(defvar-local beatmacs--render-timer nil)
(defvar beatmacs--flip nil "Alternates the loop file name.")

(defun beatmacs--sel-layer ()
  "The selected layer."
  (nth beatmacs--layer (beatmacs-track-layers beatmacs--track)))

;;;; Option & page registries

(defvar beatmacs--options (make-hash-table :test #'eq))
(defvar beatmacs--pages nil "Alist of (ID . PLIST) in tab order.")

(defun beatmacs-define-option (id &rest plist)
  "Define option ID.
PLIST keys (functions get TRACK LAYER ARG, setters also VALUE):
  :label   string or function -> string
  :kind    `number' stepper, `enum' stepper over :choices,
           `choice' stacked buttons, `toggle', or `action'
  :get     (TRACK LAYER ARG) -> value
  :set     (TRACK LAYER VALUE ARG)
  :min :max :step :big   numbers (for `number')
  :choices list of (VALUE . LABEL) or function -> such list
  :format  (VALUE TRACK LAYER) -> display string
  :action  (TRACK LAYER ARG) for `action'
  :help    explanation shown on the help screen
Columns refer to options as ID or (ID ARG)."
  (puthash id plist beatmacs--options)
  id)

(defun beatmacs-define-page (id &rest plist)
  "Define page ID.
PLIST: :label (tab text), :title, :columns (list of (TITLE REF...)
or function (TRACK LAYER) -> that), :tab (non-nil = shown as a tab
in the bottom row; nil = reachable via its own button, like Config)."
  (beatmacs--register 'beatmacs--pages id plist))

(defmacro beatmacs--layer-option (id key &rest plist)
  "Define option ID editing the selected layer's param KEY."
  `(beatmacs-define-option
    ',id
    :get (lambda (_tr ly _a) (beatmacs-param ly ,key))
    :set (lambda (_tr ly v _a) (beatmacs-set-param ly ,key v))
    ,@plist))

(defun beatmacs--ref (ref)
  "Split column entry REF into (SPEC . ARG)."
  (let ((id (if (consp ref) (car ref) ref)))
    (cons (or (gethash id beatmacs--options)
              (error "beatmacs: unknown option %S" id))
          (and (consp ref) (cadr ref)))))

(defun beatmacs--call (spec key arg &rest extra)
  "Call SPEC's KEY function with track, layer, EXTRA values and ARG."
  (apply (plist-get spec key) beatmacs--track (beatmacs--sel-layer)
         (append extra (list arg))))

(defun beatmacs--label (spec arg)
  (let ((l (plist-get spec :label)))
    (if (functionp l) (funcall l beatmacs--track (beatmacs--sel-layer) arg) l)))

(defun beatmacs--choices (spec arg)
  (let ((c (plist-get spec :choices)))
    (if (functionp c) (funcall c beatmacs--track (beatmacs--sel-layer) arg) c)))

(defun beatmacs--display (spec arg)
  "Current value of SPEC as display text."
  (let ((v (beatmacs--call spec :get arg))
        (f (plist-get spec :format)))
    (cond (f (funcall f v beatmacs--track (beatmacs--sel-layer)))
          ((memq (plist-get spec :kind) '(enum choice))
           (or (cdr (assoc v (beatmacs--choices spec arg))) (format "%s" v)))
          (t (format "%s" v)))))

;;;; Changing values

(defun beatmacs--set (spec arg value)
  (beatmacs--call spec :set arg value)
  (beatmacs--changed))

(defun beatmacs--step (spec arg dir big)
  "Move SPEC's value one step in DIR (+1/-1); BIG = large step."
  (pcase (plist-get spec :kind)
    ('number
     (let* ((step (if big (or (plist-get spec :big) (plist-get spec :step))
                    (plist-get spec :step)))
            (cur (or (beatmacs--call spec :get arg)
                     (and (plist-get spec :default)
                          (funcall (plist-get spec :default)
                                   beatmacs--track (beatmacs--sel-layer)))
                     0))
            (new (max (plist-get spec :min)
                      (min (plist-get spec :max) (+ cur (* dir step))))))
       (beatmacs--set spec arg (if (integerp step) (round new)
                                 (/ (fround (* new 1000)) 1000)))))
    ((or 'enum 'choice)
     (let* ((cs (beatmacs--choices spec arg))
            (i (or (cl-position (beatmacs--call spec :get arg) cs
                                :key #'car :test #'equal)
                   0)))
       (beatmacs--set spec arg (car (nth (mod (+ i dir) (length cs)) cs)))))
    ('toggle (beatmacs--set spec arg (not (beatmacs--call spec :get arg))))))

(defun beatmacs--prompt (spec arg)
  "Ask for a new value of SPEC directly."
  (pcase (plist-get spec :kind)
    ('number
     (let ((v (read-number (format "%s: " (beatmacs--label spec arg))
                           (beatmacs--call spec :get arg))))
       (beatmacs--set spec arg
                      (max (plist-get spec :min) (min (plist-get spec :max) v)))))
    ((or 'enum 'choice)
     (let* ((cs (beatmacs--choices spec arg))
            (pick (completing-read (format "%s: " (beatmacs--label spec arg))
                                   (mapcar #'cdr cs) nil t)))
       (beatmacs--set spec arg (car (rassoc pick cs)))))))

(defun beatmacs--option-at-point ()
  (get-text-property (point) 'beatmacs-option))

(defun beatmacs-increase (&optional big)
  "Increase the option at point.  With prefix BIG, use a big step."
  (interactive "P")
  (let ((o (or (beatmacs--option-at-point) (user-error "No option here"))))
    (beatmacs--step (car o) (cdr o) 1 big)))

(defun beatmacs-decrease (&optional big)
  "Decrease the option at point.  With prefix BIG, use a big step."
  (interactive "P")
  (let ((o (or (beatmacs--option-at-point) (user-error "No option here"))))
    (beatmacs--step (car o) (cdr o) -1 big)))

;;;; Options: Beat page

(defun beatmacs--genre-choices (&rest _)
  (mapcar (lambda (g) (cons (car g) (plist-get (cdr g) :label))) beatmacs--genres))

(beatmacs-define-option 'genre
  :label "Genre" :kind 'choice :choices #'beatmacs--genre-choices
  :get (lambda (tr _ _) (beatmacs-track-genre tr))
  :set (lambda (tr _ v _) (beatmacs-apply-style tr v))
  :help "Picks the style.  Loads its tempo, swing, key and first pattern.")

(beatmacs-define-option 'variant
  :label "Pattern" :kind 'choice
  :choices (lambda (tr _ _)
             (mapcar (lambda (v) (cons (car v) (plist-get (cdr v) :label)))
                     (plist-get (beatmacs-genre (beatmacs-track-genre tr))
                                :variants)))
  :get (lambda (tr _ _) (beatmacs-track-variant tr))
  :set (lambda (tr _ v _) (beatmacs-apply-style tr (beatmacs-track-genre tr) v))
  :help "Drum and bass pattern within the genre.  Resets tempo and swing \
to the pattern's defaults.")

(beatmacs-define-option 'bpm
  :label "Tempo" :kind 'number :min 50 :max 200 :step 1 :big 10
  :get (lambda (tr _ _) (beatmacs-track-bpm tr))
  :set (lambda (tr _ v _) (setf (beatmacs-track-bpm tr) v))
  :format (lambda (v &rest _) (format "%d BPM" v))
  :help "Beats per minute.  Boom bap ~90, R&B ~72, dubstep/trap 140 \
(felt half-time), DnB 174.")

(beatmacs-define-option 'swing
  :label "Swing" :kind 'number :min 0 :max 100 :step 5 :big 20
  :get (lambda (tr _ _) (beatmacs-track-swing tr))
  :set (lambda (tr _ v _) (setf (beatmacs-track-swing tr) v))
  :format (lambda (v &rest _) (format "%d%%" v))
  :help "Delays every second 16th note.  0 = straight/robotic, 20-40 = \
laid-back hip-hop groove, 100 = triplet shuffle.")

(beatmacs-define-option 'bars
  :label "Loop length" :kind 'enum
  :choices '((1 . "1 bar") (2 . "2 bars") (4 . "4 bars") (8 . "8 bars"))
  :get (lambda (tr _ _) (beatmacs-track-bars tr))
  :set (lambda (tr _ v _) (setf (beatmacs-track-bars tr) v))
  :help "How many bars before the loop repeats.  The chord progression \
needs 4 bars to play through once.")

(beatmacs-define-option 'key
  :label "Key" :kind 'enum
  :choices (lambda (&rest _)
             (cl-loop for i below 12
                      collect (cons i (concat (aref beatmacs-note-names i)
                                              " minor"))))
  :get (lambda (tr _ _) (beatmacs-track-key tr))
  :set (lambda (tr _ v _) (setf (beatmacs-track-key tr) v))
  :help "Musical key of the bass.  Move it to fit your voice when you \
sing a hook.")

;;;; Options: Sound page (drums)

(beatmacs--layer-option kick-pitch :kick-pitch
  :label "Kick pitch" :kind 'number :min 35 :max 90 :step 1 :big 5
  :format (lambda (v &rest _) (format "%d Hz" v))
  :help "Where the kick's pitch sweep ends.  Low = boomy, high = punchy/tight.")

(beatmacs--layer-option kick-decay :kick-decay
  :label "Kick length" :kind 'number :min 0.1 :max 1.2 :step 0.05 :big 0.2
  :format (lambda (v &rest _) (format "%.2f s" v))
  :help "How long the kick rings.  Short kicks leave room for the bass.")

(beatmacs--layer-option snare-sound :snare
  :label "Backbeat" :kind 'enum
  :choices '((auto . "auto") (snare . "snare") (clap . "clap") (both . "snare+clap"))
  :help "Sound on the snare lane.  auto = what the pattern suggests.")

(beatmacs--layer-option hats :hats
  :label "Hi-hats" :kind 'enum
  :choices '((pattern . "pattern") (8ths . "8ths") (16ths . "16ths")
             (rolls . "trap rolls") (off . "off"))
  :help "Override the hi-hats.  16ths add energy, rolls are the trap \
signature, off is great for an intro.")

(beatmacs--layer-option hat-decay :hat-decay
  :label "Hat length" :kind 'number :min 0.01 :max 0.12 :step 0.005 :big 0.02
  :format (lambda (v &rest _) (format "%d ms" (round (* v 1000))))
  :help "Closed hi-hat length.  Short = tight, long = washy.")

;;;; Options: Sound page (bass)

(beatmacs--layer-option bass-type :type
  :label "Instrument" :kind 'enum
  :choices (lambda (tr _ _)
             (let ((auto (or (beatmacs-style-get tr :bass-type) 'sub)))
               `((auto . ,(format "auto (%s)" auto))
                 (808 . "808") (sub . "sub") (reese . "reese")
                 (wobble . "wobble"))))
  :help "808 = sine with long decay (trap), sub = clean sine, reese = \
detuned saws (DnB), wobble = LFO-filtered saw (dubstep).")

(beatmacs--layer-option octave :octave
  :label "Octave" :kind 'number :min 0 :max 3 :step 1
  :format (lambda (v tr _)
            (let ((n (+ (* 12 (1+ v)) (beatmacs-track-key tr))))
              (format "%s%d %dHz" (aref beatmacs-note-names (% n 12))
                      (1- (/ n 12)) (round (beatmacs-synth-midi-to-hz n)))))
  :help "Bass register.  1 is the club sub-bass range; phone speakers \
start working at about 80-100 Hz, so try 2 when listening without \
headphones.")

(beatmacs--layer-option decay :decay
  :label "808 decay" :kind 'number :min 0.1 :max 2.0 :step 0.1
  :format (lambda (v &rest _) (format "%.1f s" v))
  :help "How long the 808 rings out.")

(beatmacs--layer-option drive :drive
  :label "Drive" :kind 'number :min 1.0 :max 6.0 :step 0.2 :big 1.0
  :format (lambda (v &rest _) (format "%.1f×" v))
  :help "Saturation.  Adds harmonics: grit for wobbles, and makes 808s \
audible on small speakers.")

(beatmacs--layer-option cutoff :cutoff
  :label "Cutoff" :kind 'number :min 150 :max 4000 :step 50 :big 500
  :default #'beatmacs-bass-cutoff
  :format (lambda (v tr ly)
            (if v (format "%d Hz" v)
              (format "auto %d" (beatmacs-bass-cutoff tr ly))))
  :help "Low-pass filter: frequencies above the cutoff are removed.  \
Lower = darker/rounder, higher = brighter/buzzier.  For the wobble it \
is the highest point of the sweep.")

(beatmacs--layer-option resonance :resonance
  :label "Resonance" :kind 'number :min 0.0 :max 0.95 :step 0.05 :big 0.2
  :format (lambda (v &rest _) (format "%d%%" (round (* v 100))))
  :help "Boosts frequencies right at the cutoff - makes the filter \
sweep sound vocal (\"yoy\").")

(beatmacs--layer-option wobble-rate :rate
  :label "Wobble rate" :kind 'enum
  :choices '((pattern . "pattern") (0.5 . "1/2") (1 . "1/4") (2 . "1/8")
             (3 . "1/8T") (4 . "1/16") (6 . "1/16T") (8 . "1/32"))
  :help "LFO speed, locked to the tempo.  pattern = each note uses its \
own rate (that's what makes the bass 'talk').  T = triplet.")

(beatmacs--layer-option detune :detune
  :label "Detune" :kind 'number :min 0 :max 60 :step 1 :big 5
  :format (lambda (v &rest _) (format "%d cent" v))
  :help "Distance between the two reese saws.  Small = slow phasing, \
large = fast, nasty growl.")

;;;; Options: Mix page

(beatmacs-define-option 'volume
  :label (lambda (tr _ i) (beatmacs-layer-name (nth i (beatmacs-track-layers tr))))
  :kind 'number :min 0.0 :max 1.5 :step 0.05 :big 0.2
  :get (lambda (tr _ i) (beatmacs-layer-volume (nth i (beatmacs-track-layers tr))))
  :set (lambda (tr _ v i)
         (setf (beatmacs-layer-volume (nth i (beatmacs-track-layers tr))) v))
  :format (lambda (v &rest _) (format "%d%%" (round (* v 100))))
  :help "Layer volume.")

(beatmacs-define-option 'mute
  :label (lambda (tr _ i)
           (concat "Mute " (beatmacs-layer-name (nth i (beatmacs-track-layers tr)))))
  :kind 'toggle
  :get (lambda (tr _ i) (beatmacs-layer-mute (nth i (beatmacs-track-layers tr))))
  :set (lambda (tr _ v i)
         (setf (beatmacs-layer-mute (nth i (beatmacs-track-layers tr))) v))
  :help "Silence a layer - e.g. drop the drums for an intro.")

(beatmacs-define-option 'master
  :label "Master" :kind 'number :min 0.3 :max 3.0 :step 0.1 :big 0.5
  :get (lambda (tr _ _) (beatmacs-track-master tr))
  :set (lambda (tr _ v _) (setf (beatmacs-track-master tr) v))
  :format (lambda (v &rest _) (format "%.1f×" v))
  :help "Gain into the final saturator.  Higher = louder and more \
compressed/distorted.")

(beatmacs-define-option 'remove-layer
  :label "Remove selected" :kind 'action
  :action (lambda (&rest _) (beatmacs-remove-layer))
  :help "Delete the layer selected in the track list.")

;;;; Options: Config page

(beatmacs-define-option 'save
  :label "Save preset" :kind 'action :action (lambda (&rest _) (call-interactively #'beatmacs-save-preset))
  :help "Save all settings under a name.")
(beatmacs-define-option 'load
  :label "Load preset" :kind 'action :action (lambda (&rest _) (call-interactively #'beatmacs-load-preset))
  :help "Load saved settings.")
(beatmacs-define-option 'export
  :label "Export WAV" :kind 'action :action (lambda (&rest _) (beatmacs-export))
  :help "Write a long version of the loop to `beatmacs-export-directory'.")
(beatmacs-define-option 'rename
  :label "Rename track" :kind 'action :action (lambda (&rest _) (call-interactively #'beatmacs-rename))
  :help "Change the track name (used for presets and exports).")

(beatmacs-define-option 'export-length
  :label "Export length" :kind 'number :min 10 :max 900 :step 10 :big 60
  :get (lambda (&rest _) beatmacs-export-seconds)
  :set (lambda (_ _ v _) (setq beatmacs-export-seconds v))
  :format (lambda (v &rest _) (format "%d:%02d" (/ v 60) (% v 60)))
  :help "Length of exports in minutes:seconds.")

(beatmacs-define-option 'backend
  :label "Engine" :kind 'enum
  :choices (lambda (&rest _)
             (mapcar (lambda (b) (cons (car b) (plist-get (cdr b) :label)))
                     beatmacs--backends))
  :get (lambda (&rest _) beatmacs-backend)
  :set (lambda (_ _ v _) (setq beatmacs-backend v))
  :help "Sound engine.  New ones plug in via `beatmacs-define-backend'.")

(beatmacs-define-option 'sample-rate
  :label "Sample rate" :kind 'enum
  :choices '((11025 . "11 kHz fast") (22050 . "22 kHz") (32000 . "32 kHz")
             (44100 . "44 kHz best"))
  :get (lambda (&rest _) beatmacs-sample-rate)
  :set (lambda (_ _ v _) (setq beatmacs-sample-rate v))
  :help "Playback render quality.  Lower renders faster on slow phones.")

(beatmacs-define-option 'player
  :label "Player" :kind 'enum
  :choices (lambda (&rest _)
             (cons (cons 'auto (format "auto (%s)" (beatmacs-player-resolve)))
                   (mapcar (lambda (p) (cons (car p) (symbol-name (car p))))
                           beatmacs-players)))
  :get (lambda (&rest _) beatmacs-player)
  :set (lambda (_ _ v _) (beatmacs-player-stop) (setq beatmacs-player v))
  :help "Audio player.  On Android: install mpv in Termux (F-Droid \
builds of Emacs and Termux), otherwise android-open hands the file to \
your media app.")

(beatmacs-define-option 'auto-render
  :label "Live re-render" :kind 'toggle
  :get (lambda (&rest _) beatmacs-auto-render)
  :set (lambda (_ _ v _) (setq beatmacs-auto-render v))
  :help "Re-render and restart the loop after each change while playing.")

;;;; Pages

(beatmacs-define-page 'beat
  :label "Beat" :tab t
  :columns '(("Style" genre variant) ("Groove" bpm swing key bars)))

(beatmacs-define-page 'sound
  :label "Sound" :tab t
  :columns (lambda (tr ly)
             (funcall (plist-get (beatmacs-layer-spec (beatmacs-layer-type ly))
                                 :columns)
                      tr ly)))

(beatmacs-define-page 'mix
  :label "Mix" :tab t
  :columns (lambda (tr _)
             (let ((n (length (beatmacs-track-layers tr))))
               `(("Levels" ,@(cl-loop for i below n collect `(volume ,i)) master)
                 ("Mute" ,@(cl-loop for i below n collect `(mute ,i))
                  remove-layer)))))

(beatmacs-define-page 'config
  :label "Config" :tab nil
  :columns '(("File" save load export export-length rename)
             ("Engine" backend sample-rate player auto-render)))

;;;; Rendering the panel

(defun beatmacs--pad (s w &optional center)
  "Pad or cut S to display width W; CENTER it if non-nil."
  (let* ((s (truncate-string-to-width s w))
         (gap (- w (string-width s))))
    (if center
        (concat (make-string (/ gap 2) ?\s) s (make-string (- gap (/ gap 2)) ?\s))
      (concat s (make-string gap ?\s)))))

(defun beatmacs--button (label id action &rest props)
  "A button string showing LABEL with symbol ID running ACTION.
PROPS are extra text properties (e.g. face, beatmacs-option)."
  (apply #'make-text-button (copy-sequence label) nil
         'action (lambda (_) (funcall action))
         'follow-link t
         'face 'beatmacs-button
         'mouse-face 'highlight
         'beatmacs-id id
         props))

(defun beatmacs--id (&rest parts)
  (intern (mapconcat (lambda (p) (format "%s" p)) parts ":")))

(defun beatmacs--render-option (ref w)
  "Lines (strings) for column entry REF at width W."
  (pcase-let* ((`(,spec . ,arg) (beatmacs--ref ref))
               (label (beatmacs--label spec arg))
               (kind (plist-get spec :kind))
               (oprop (cons spec arg))
               (base (if (consp ref) (format "%s" ref) (symbol-name ref))))
    (pcase kind
      ('choice
       (let ((cur (beatmacs--call spec :get arg)))
         (cons (propertize (beatmacs--pad label w) 'face 'beatmacs-dim)
               (mapcar
                (lambda (c)
                  (let ((sel (equal (car c) cur)))
                    (beatmacs--button
                     (beatmacs--pad (concat (if sel " ▸ " "   ") (cdr c)) w)
                     (beatmacs--id base (car c))
                     (lambda () (beatmacs--set spec arg (car c)))
                     'face (if sel 'beatmacs-selected 'beatmacs-button)
                     'beatmacs-option oprop
                     'help-echo (plist-get spec :help))))
                (beatmacs--choices spec arg)))))
      ((or 'number 'enum)
       (list (propertize (beatmacs--pad label w) 'face 'beatmacs-dim)
             (concat
              (beatmacs--button " − " (beatmacs--id base '-)
                                (lambda () (beatmacs--step spec arg -1 nil))
                                'beatmacs-option oprop)
              (beatmacs--button (beatmacs--pad (beatmacs--display spec arg)
                                               (max 1 (- w 6)) t)
                                (beatmacs--id base '=)
                                (lambda () (beatmacs--prompt spec arg))
                                'face 'default
                                'beatmacs-option oprop
                                'help-echo "Tap to type a value")
              (beatmacs--button " + " (beatmacs--id base '+)
                                (lambda () (beatmacs--step spec arg 1 nil))
                                'beatmacs-option oprop))))
      ('toggle
       (let ((on (beatmacs--call spec :get arg)))
         (list (beatmacs--button
                (beatmacs--pad (concat (if on " [x] " " [ ] ") label) w)
                (beatmacs--id base)
                (lambda () (beatmacs--step spec arg 1 nil))
                'face (if on 'beatmacs-selected 'beatmacs-button)
                'beatmacs-option oprop))))
      ('action
       (list (beatmacs--button
              (beatmacs--pad label w t) (beatmacs--id base)
              (lambda () (funcall (plist-get spec :action)
                                  beatmacs--track (beatmacs--sel-layer) arg))))))))

(defun beatmacs--render-column (col w)
  "Lines for column COL = (TITLE REF...) at width W."
  (append (list (propertize (beatmacs--pad (car col) w) 'face 'beatmacs-title)
                (propertize (make-string w ?─) 'face 'beatmacs-dim))
          (cl-loop for ref in (cdr col)
                   append (append (beatmacs--render-option ref w) (list "")))))

(defun beatmacs--page-columns ()
  (let ((c (plist-get (cdr (assq beatmacs--page beatmacs--pages)) :columns)))
    (if (functionp c) (funcall c beatmacs--track (beatmacs--sel-layer)) c)))

(defun beatmacs--width ()
  (let ((win (get-buffer-window (current-buffer))))
    (max 20 (1- (if win (window-body-width win) 44)))))

(defun beatmacs--body-lines (width)
  "Lines of the page area for WIDTH."
  (let* ((cols (beatmacs--page-columns))
         (cw (/ (- width 3) 2)))
    (if (or (< cw 14) (/= (length cols) 2))
        (cl-loop for col in cols append (beatmacs--render-column col width))
      (let* ((a (beatmacs--render-column (nth 0 cols) cw))
             (b (beatmacs--render-column (nth 1 cols) cw))
             (n (max (length a) (length b))))
        (cl-loop for i below n
                 collect (concat (beatmacs--pad (or (nth i a) "") cw)
                                 (propertize " │ " 'face 'beatmacs-dim)
                                 (or (nth i b) "")))))))

(defun beatmacs--header-lines (width)
  (let* ((tr beatmacs--track)
         (g (beatmacs-genre (beatmacs-track-genre tr)))
         (playing (beatmacs-player-playing-p))
         (bw (/ (- width 2) 3)))
    (list
     (propertize (beatmacs--pad (format " ♪ %s" (beatmacs-track-name tr)) width)
                 'face 'beatmacs-title)
     (propertize
      (format "   %s · %s · %d BPM · %s minor"
              (plist-get g :label) (plist-get (beatmacs-variant tr) :label)
              (beatmacs-track-bpm tr)
              (aref beatmacs-note-names (beatmacs-track-key tr)))
      'face 'beatmacs-dim)
     (concat
      (beatmacs--button (beatmacs--pad (if playing "■ Stop" "▶ Play") bw t)
                        'transport:play #'beatmacs-toggle-play
                        'face (if playing 'beatmacs-selected 'beatmacs-button))
      " "
      (beatmacs--button (beatmacs--pad "↻ Render" bw t)
                        'transport:render #'beatmacs-render-and-play)
      " "
      (beatmacs--button (beatmacs--pad "? Help" bw t)
                        'transport:help #'beatmacs-help)))))

(defun beatmacs--layer-lines (width)
  (let ((pw (max 4 (- width 13))))
    (append
     (cl-loop for layer in (beatmacs-track-layers beatmacs--track)
              for i from 0
              collect
              (let* ((sel (= i beatmacs--layer))
                     (spec (beatmacs-layer-spec (beatmacs-layer-type layer)))
                     (prev (if (beatmacs-layer-mute layer) "(muted)"
                             (funcall (plist-get spec :preview)
                                      beatmacs--track layer pw)))
                     (idx i))
                (beatmacs--button
                 (beatmacs--pad (format "%s%-9s %s" (if sel " ▸ " "   ")
                                        (beatmacs-layer-name layer) prev)
                                width)
                 (beatmacs--id 'layer i)
                 (lambda () (beatmacs-select-layer idx))
                 'face (if sel 'beatmacs-selected 'beatmacs-button)
                 'help-echo "Select layer (edit it on the Sound page)")))
     (list (beatmacs--button (beatmacs--pad "+ Layer" (min width 14) t)
                             'layer:add #'beatmacs-add-layer)))))

(defun beatmacs--tab-lines (width)
  (let* ((tabs (cl-remove-if-not (lambda (p) (plist-get (cdr p) :tab))
                                 beatmacs--pages))
         (n (length tabs))
         (tw (/ (- width (1- n)) n)))
    (list
     (mapconcat
      (lambda (p)
        (let ((i (1+ (cl-position p tabs))))
          (beatmacs--button
           (beatmacs--pad (format "P%d %s" i (plist-get (cdr p) :label)) tw t)
           (beatmacs--id 'tab (car p))
           (lambda () (beatmacs-goto-page (car p)))
           'face (if (eq beatmacs--page (car p)) 'beatmacs-selected
                   'beatmacs-button))))
      tabs " ")
     (concat (make-string (/ (- width 16) 2) ?\s)
             (beatmacs--button (beatmacs--pad "⚙ Config" 16 t) 'tab:config
                               (lambda () (beatmacs-goto-page 'config))
                               'face (if (eq beatmacs--page 'config)
                                         'beatmacs-selected 'beatmacs-button))))))

(defun beatmacs--window-lines ()
  "How many panel lines fit in the window (accounting for spacing)."
  (let ((win (get-buffer-window (current-buffer))))
    (if (not win) 0
      (let* ((ch (frame-char-height))
             (ls (cond ((not (display-graphic-p)) 0) ; ttys ignore spacing
                       ((floatp line-spacing) (* ch line-spacing))
                       (t (or line-spacing 0)))))
        (floor (window-body-height win t) (+ ch ls))))))

(defun beatmacs-redraw ()
  "Redraw the panel, keeping point on the same button."
  (interactive)
  (let* ((inhibit-read-only t)
         (id (get-text-property (point) 'beatmacs-id))
         (width (beatmacs--width))
         (sep (propertize (make-string width ?─) 'face 'beatmacs-dim))
         (top (append (beatmacs--header-lines width) (list sep)
                      (beatmacs--layer-lines width) (list sep)
                      (beatmacs--body-lines width)))
         (bottom (cons sep (beatmacs--tab-lines width)))
         (pad (max 0 (- (beatmacs--window-lines) (length top) (length bottom) 1))))
    (erase-buffer)
    (insert (mapconcat #'identity top "\n") "\n"
            (make-string pad ?\n)
            (mapconcat #'identity bottom "\n"))
    (goto-char (or (and id (text-property-any (point-min) (point-max)
                                              'beatmacs-id id))
                   (point-min)))))

;;;; Commands

(defun beatmacs--changed ()
  "Called after any change: redraw and maybe re-render."
  (beatmacs-redraw)
  (when (and beatmacs-auto-render (beatmacs-player-playing-p))
    (when (timerp beatmacs--render-timer) (cancel-timer beatmacs--render-timer))
    (let ((buf (current-buffer)))
      (setq beatmacs--render-timer
            (run-with-idle-timer
             0.6 nil
             (lambda ()
               (when (buffer-live-p buf)
                 (with-current-buffer buf (beatmacs-render-and-play)))))))))

(defun beatmacs--loop-file ()
  (setq beatmacs--flip (not beatmacs--flip))
  (expand-file-name (if beatmacs--flip "loop-a.wav" "loop-b.wav")
                    (expand-file-name "cache/" beatmacs-directory)))

(defun beatmacs-render-and-play ()
  "Render the current track and loop it."
  (interactive)
  (message "beatmacs: rendering...")
  (redisplay t)
  (let* ((t0 (float-time))
         (file (beatmacs-render beatmacs--track (beatmacs--loop-file)))
         (dt (- (float-time) t0))
         (player (beatmacs-player-play file)))
    (message "beatmacs: rendered %.1fs loop in %.1fs, playing with %s"
             (beatmacs-loop-seconds beatmacs--track) dt player))
  (beatmacs-redraw))

(defun beatmacs-toggle-play ()
  "Start or stop the loop."
  (interactive)
  (if (beatmacs-player-playing-p)
      (progn (beatmacs-player-stop) (beatmacs-redraw))
    (beatmacs-render-and-play)))

(defun beatmacs-goto-page (page)
  "Show PAGE."
  (interactive (list (intern (completing-read "Page: " (mapcar #'car beatmacs--pages)))))
  (setq beatmacs--page page)
  (beatmacs-redraw)
  (goto-char (point-min)))

(defun beatmacs--tab-command (n)
  (lambda () (interactive)
    (let ((tabs (cl-remove-if-not (lambda (p) (plist-get (cdr p) :tab))
                                  beatmacs--pages)))
      (when (nth (1- n) tabs) (beatmacs-goto-page (car (nth (1- n) tabs)))))))

(defun beatmacs-select-layer (i)
  "Select layer I and show its Sound page."
  (setq beatmacs--layer i
        beatmacs--page (if (eq beatmacs--page 'beat) 'sound beatmacs--page))
  (beatmacs-redraw))

(defun beatmacs-next-layer (dir)
  "Select the next layer (DIR = 1) or previous (-1)."
  (interactive (list 1))
  (beatmacs-select-layer (mod (+ beatmacs--layer dir)
                              (length (beatmacs-track-layers beatmacs--track)))))

(defun beatmacs-prev-layer ()
  "Select the previous layer."
  (interactive)
  (beatmacs-next-layer -1))

(defun beatmacs-add-layer (&optional type)
  "Add a layer of TYPE (asks)."
  (interactive)
  (let* ((type (or type
                   (intern (completing-read
                            "Layer type: "
                            (mapcar (lambda (l) (symbol-name (car l)))
                                    beatmacs--layer-types)
                            nil t))))
         (layers (beatmacs-track-layers beatmacs--track)))
    (setf (beatmacs-track-layers beatmacs--track)
          (append layers (list (beatmacs-make-layer type))))
    (setq beatmacs--layer (length layers))
    (beatmacs--changed)))

(defun beatmacs-remove-layer ()
  "Remove the selected layer."
  (interactive)
  (let ((layer (beatmacs--sel-layer)))
    (when (and (cdr (beatmacs-track-layers beatmacs--track))
               (y-or-n-p (format "Remove layer %s? " (beatmacs-layer-name layer))))
      (setf (beatmacs-track-layers beatmacs--track)
            (delq layer (beatmacs-track-layers beatmacs--track)))
      (setq beatmacs--layer 0)
      (beatmacs--changed))))

(defun beatmacs-rename (name)
  "Rename the track to NAME."
  (interactive (list (read-string "Track name: " (beatmacs-track-name beatmacs--track))))
  (setf (beatmacs-track-name beatmacs--track) name)
  (beatmacs-redraw))

(defun beatmacs-save-preset (name)
  "Save the track as preset NAME."
  (interactive (list (read-string "Preset name: " (beatmacs-track-name beatmacs--track))))
  (let ((file (expand-file-name (concat name ".eld") beatmacs-preset-directory)))
    (make-directory beatmacs-preset-directory t)
    (with-temp-file file
      (insert ";; beatmacs preset -*- mode: lisp-data -*-\n")
      (pp (beatmacs-track-to-plist beatmacs--track) (current-buffer)))
    (message "beatmacs: saved %s" file)))

(defun beatmacs-load-preset (name)
  "Load preset NAME."
  (interactive
   (list (completing-read
          "Preset: "
          (and (file-directory-p beatmacs-preset-directory)
               (mapcar #'file-name-sans-extension
                       (directory-files beatmacs-preset-directory nil "\\.eld\\'")))
          nil t)))
  (let ((file (expand-file-name (concat name ".eld") beatmacs-preset-directory)))
    (setq beatmacs--track
          (beatmacs-track-from-plist
           (with-temp-buffer (insert-file-contents file) (read (current-buffer))))
          beatmacs--layer 0)
    (beatmacs--changed)
    (message "beatmacs: loaded %s" name)))

(defun beatmacs--export-dir ()
  (let ((d (expand-file-name beatmacs-export-directory)))
    (condition-case nil (progn (make-directory d t) d)
      (error (expand-file-name "exports/" beatmacs-directory)))))

(defun beatmacs-export ()
  "Export the track as a WAV of `beatmacs-export-seconds'."
  (interactive)
  (let* ((tr beatmacs--track)
         (file (expand-file-name
                (format "%s-%s-%dbpm.wav"
                        (replace-regexp-in-string "[^[:alnum:]_-]" "_"
                                                  (beatmacs-track-name tr))
                        (beatmacs-track-variant tr) (beatmacs-track-bpm tr))
                (beatmacs--export-dir))))
    (message "beatmacs: exporting...")
    (redisplay t)
    (beatmacs-render tr file beatmacs-export-seconds beatmacs-export-sample-rate)
    (message "beatmacs: exported %s" file)
    file))

;;;; Help

(defun beatmacs--help-insert-option (ref)
  (pcase-let ((`(,spec . ,arg) (beatmacs--ref ref)))
    (when (plist-get spec :help)
      (insert (propertize (beatmacs--label spec arg) 'face 'bold) " - "
              (plist-get spec :help) "\n\n"))))

(defun beatmacs-help ()
  "Learning notes for the current genre, pattern and page."
  (interactive)
  (let* ((tr beatmacs--track)
         (g (beatmacs-genre (beatmacs-track-genre tr)))
         (v (beatmacs-variant tr))
         (cols (beatmacs--page-columns))
         (panel (current-buffer))
         (buf (get-buffer-create "*beatmacs help*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (special-mode)
        (setq-local line-spacing beatmacs-line-spacing)
        (visual-line-mode 1)
        (insert-text-button " ← Back " 'follow-link t 'face 'beatmacs-button
                            'action (lambda (_) (quit-window nil)))
        (insert "\n\n")
        (insert (propertize (plist-get g :label) 'face 'beatmacs-title) "\n"
                (plist-get g :notes) "\n\n")
        (insert (propertize (concat "Pattern: " (plist-get v :label))
                            'face 'beatmacs-title)
                "\n" (or (plist-get v :notes) "") "\n\n")
        (insert (propertize "Rapping over it" 'face 'beatmacs-title) "\n"
                (plist-get g :rap-tips) "\n\n")
        (insert (propertize "Controls on this page" 'face 'beatmacs-title) "\n")
        (let ((beatmacs--track tr)
              (beatmacs--layer (buffer-local-value 'beatmacs--layer panel)))
          (dolist (col cols) (mapc #'beatmacs--help-insert-option (cdr col))))
        (insert beatmacs-primer "\n\n"
                (propertize "Keys" 'face 'beatmacs-title) "\n"
                (substitute-command-keys "\\{beatmacs-mode-map}"))
        (goto-char (point-min))))
    (pop-to-buffer-same-window buf)))

;;;; Mode

(defvar-keymap beatmacs-mode-map
  :doc "Keymap for `beatmacs-mode'."
  :parent special-mode-map
  "TAB" #'forward-button
  "<backtab>" #'backward-button
  "n" #'forward-button
  "p" #'backward-button
  "+" #'beatmacs-increase
  "=" #'beatmacs-increase
  "-" #'beatmacs-decrease
  "SPC" #'beatmacs-toggle-play
  "r" #'beatmacs-render-and-play
  "1" (beatmacs--tab-command 1)
  "2" (beatmacs--tab-command 2)
  "3" (beatmacs--tab-command 3)
  "c" (lambda () (interactive) (beatmacs-goto-page 'config))
  ">" #'beatmacs-next-layer
  "<" #'beatmacs-prev-layer
  "a" #'beatmacs-add-layer
  "?" #'beatmacs-help
  "s" #'beatmacs-save-preset
  "o" #'beatmacs-load-preset
  "e" #'beatmacs-export
  "g" #'beatmacs-redraw
  "q" #'beatmacs-quit)

(define-derived-mode beatmacs-mode special-mode "Beatmacs"
  "Beat generator panel.

\\{beatmacs-mode-map}"
  (setq-local line-spacing beatmacs-line-spacing
              truncate-lines t
              revert-buffer-function (lambda (&rest _) (beatmacs-redraw)))
  (add-hook 'window-size-change-functions
            (lambda (win)
              (with-current-buffer (window-buffer win)
                (when (eq major-mode 'beatmacs-mode)
                  (with-selected-window win (beatmacs-redraw)))))
            nil t))

(defun beatmacs--on-player-stopped ()
  "Refresh the panel's Play button when playback ends by itself."
  (when-let ((buf (get-buffer "*beatmacs*")))
    (with-current-buffer buf
      (when (eq major-mode 'beatmacs-mode) (beatmacs-redraw)))))

(add-hook 'beatmacs-player-stopped-hook #'beatmacs--on-player-stopped)

(defun beatmacs-quit ()
  "Stop playback and bury the panel."
  (interactive)
  (beatmacs-player-stop)
  (quit-window))

;;;###autoload
(defun beatmacs ()
  "Open the beatmacs panel."
  (interactive)
  (let ((buf (get-buffer-create "*beatmacs*")))
    (pop-to-buffer-same-window buf)
    (unless (eq major-mode 'beatmacs-mode)
      (beatmacs-mode)
      (setq beatmacs--track (beatmacs-make-track)))
    (beatmacs-redraw)))

(provide 'beatmacs-ui)
;;; beatmacs-ui.el ends here
