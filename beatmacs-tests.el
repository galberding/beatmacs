;;; beatmacs-tests.el --- ERT tests for beatmacs -*- lexical-binding: t; -*-

;;; Commentary:

;; Run: emacs -Q --batch -L . -l beatmacs-tests.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'beatmacs)

(defun beatmacs-tests--tmp (name)
  (expand-file-name name (make-temp-file "beatmacs-test" t)))

(ert-deftest beatmacs-wav-header ()
  (let* ((file (beatmacs-tests--tmp "x.wav"))
         (pcm (beatmacs-synth-pcm16 (vector 0.0 0.5 -0.5 1.0))))
    (beatmacs-synth-write-wav file 22050 pcm 3)
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert-file-contents-literally file)
      (should (= (buffer-size) (+ 44 (* 3 8))))
      (should (string= (buffer-substring 1 5) "RIFF"))
      (should (string= (buffer-substring 9 13) "WAVE"))
      (should (string= (buffer-substring 37 41) "data")))))

(ert-deftest beatmacs-pcm-encoding ()
  (let ((s (beatmacs-synth-pcm16 (vector 1.0 -1.0 0.0))))
    (should-not (multibyte-string-p s))
    (should (equal (append s nil) '(#xFF #x7F #x01 #x80 0 0)))))

(ert-deftest beatmacs-midi-hz ()
  (should (< (abs (- (beatmacs-synth-midi-to-hz 69) 440.0)) 1e-9))
  (should (< (abs (- (beatmacs-synth-midi-to-hz 57) 220.0)) 1e-9)))

(ert-deftest beatmacs-patterns-well-formed ()
  "Every step string is a multiple of 16; bass notes fit in a bar."
  (dolist (g beatmacs--genres)
    (dolist (v (plist-get (cdr g) :variants))
      (cl-loop for (_lane s) on (plist-get (cdr v) :drums) by #'cddr
               do (should (zerop (% (length s) 16))))
      (dolist (n (plist-get (cdr v) :bass))
        (should (<= (+ (nth 0 n) (nth 1 n)) 16))))))

(ert-deftest beatmacs-events-timing ()
  (let ((tr (beatmacs-make-track 'dnb)))
    (setf (beatmacs-track-swing tr) 0)
    (should (= (beatmacs-track-bpm tr) 174))
    (let* ((kicks (cl-remove-if-not (lambda (e) (eq (plist-get e :inst) 'kick))
                                    (beatmacs-track-events tr)))
           (sd (beatmacs-step-seconds tr)))
      ;; two-step: kicks on steps 0 and 10 of each of 4 bars
      (should (= (length kicks) 8))
      (should (< (abs (- (plist-get (nth 1 kicks) :time) (* 10 sd))) 1e-9)))))

(ert-deftest beatmacs-swing-delays-odd-steps ()
  (let ((tr (beatmacs-make-track 'rap)))
    (setf (beatmacs-track-swing tr) 100)
    (should (= (beatmacs-step-time tr 2) (* 2 (beatmacs-step-seconds tr))))
    (should (> (beatmacs-step-time tr 3) (* 3 (beatmacs-step-seconds tr))))))

(ert-deftest beatmacs-mute-removes-events ()
  (let ((tr (beatmacs-make-track 'dubstep)))
    (setf (beatmacs-layer-mute (car (beatmacs-track-layers tr))) t)
    (should-not (cl-find 'kick (beatmacs-track-events tr)
                         :key (lambda (e) (plist-get e :inst))))))

(ert-deftest beatmacs-render-every-variant ()
  (dolist (g beatmacs--genres)
    (dolist (v (plist-get (cdr g) :variants))
      (let ((tr (beatmacs-make-track (car g)))
            (file (beatmacs-tests--tmp "v.wav")))
        (beatmacs-apply-style tr (car g) (car v))
        (beatmacs-render tr file nil 11025)
        (should (> (file-attribute-size (file-attributes file)) 10000))))))

(ert-deftest beatmacs-export-length ()
  (let ((tr (beatmacs-make-track 'dnb))
        (file (beatmacs-tests--tmp "long.wav")))
    (beatmacs-render tr file 30 11025)
    (let ((secs (/ (- (file-attribute-size (file-attributes file)) 44)
                   2.0 11025)))
      (should (>= secs 30))
      (should (< secs (+ 30 (beatmacs-loop-seconds tr) 0.01))))))

(ert-deftest beatmacs-preset-roundtrip ()
  (let* ((tr (beatmacs-make-track 'rap))
         (_ (beatmacs-set-param (cadr (beatmacs-track-layers tr)) :octave 2))
         (pl (beatmacs-track-to-plist tr))
         (back (beatmacs-track-from-plist
                (car (read-from-string (prin1-to-string pl))))))
    (should (equal (beatmacs-track-to-plist back) pl))))

(ert-deftest beatmacs-panel-renders-all-pages ()
  (with-temp-buffer
    (beatmacs-mode)
    (setq beatmacs--track (beatmacs-make-track))
    (dolist (layer '(0 1))
      (setq beatmacs--layer layer)
      (dolist (p beatmacs--pages)
        (setq beatmacs--page (car p))
        (beatmacs-redraw)
        (should (> (buffer-size) 100))))
    ;; every bass type has a Sound page
    (setq beatmacs--layer 1 beatmacs--page 'sound)
    (dolist (ty '(808 sub reese wobble))
      (beatmacs-set-param (beatmacs--sel-layer) :type ty)
      (beatmacs-redraw))))

(ert-deftest beatmacs-stepper-changes-value ()
  (with-temp-buffer
    (beatmacs-mode)
    (setq beatmacs--track (beatmacs-make-track 'dnb))
    (beatmacs-redraw)
    (goto-char (text-property-any (point-min) (point-max)
                                  'beatmacs-id (intern "bpm:+")))
    (push-button)
    (should (= (beatmacs-track-bpm beatmacs--track) 175))
    (beatmacs-decrease t)
    (should (= (beatmacs-track-bpm beatmacs--track) 165))
    ;; cutoff steps from its auto value
    (setq beatmacs--layer 1 beatmacs--page 'sound)
    (beatmacs-redraw)
    (beatmacs--step (gethash 'cutoff beatmacs--options) nil 1 nil)
    (should (= (beatmacs-param (beatmacs--sel-layer) :cutoff) 750))))

(ert-deftest beatmacs-add-variant-works ()
  (let ((beatmacs--genres (copy-tree beatmacs--genres)))
    (beatmacs-add-variant 'rap 'drill :label "Drill" :bpm 142
                          :drums '(:kick "X.....x...X....." :snare "............X...")
                          :bass '((0 6 0)) :progression '(0 5))
    (let ((tr (beatmacs-make-track 'rap)))
      (beatmacs-apply-style tr 'rap 'drill)
      (should (= (beatmacs-track-bpm tr) 142))
      (should (beatmacs-track-events tr)))))

(provide 'beatmacs-tests)
;;; beatmacs-tests.el ends here
