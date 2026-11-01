;;; beatmacs-player.el --- Audio playback with auto-detection -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Georg

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Plays rendered WAV loops.  Emacs itself cannot loop audio (and Emacs
;; for Android cannot play audio at all), so an external player is
;; used.  `beatmacs-player' = `auto' tries, in order:
;;
;;   mpv, ffplay, play (SoX), paplay, aplay
;;     -- found on PATH, or in Termux's bin directory on Android
;;        (works when Emacs and Termux are installed from F-Droid and
;;        share a user ID; then `pkg install mpv' in Termux is enough)
;;   android-open
;;     -- Android without Termux: copy the WAV to shared storage and
;;        open it in the system media player app (no seamless loop)
;;   emacs
;;     -- `play-sound-file' (blocks Emacs while playing; last resort)
;;
;; Players that cannot loop by themselves are restarted from the
;; process sentinel, so every external player loops.

;;; Code:

(require 'cl-lib)

(declare-function android-browse-url "androidfns.c" (url &optional send))

(defcustom beatmacs-player 'auto
  "Player used for playback.  `auto' picks the first available one."
  :type '(choice (const auto) (const mpv) (const ffplay) (const sox)
                 (const paplay) (const aplay) (const android-open)
                 (const emacs))
  :group 'beatmacs)

(defcustom beatmacs-termux-bin "/data/data/com.termux/files/usr/bin"
  "Termux binary directory, searched for players on Android."
  :type 'directory :group 'beatmacs)

(defcustom beatmacs-android-shared-directory "/sdcard/Music/beatmacs/"
  "Shared-storage directory for the `android-open' player.
Other apps can only read files here.  Emacs needs the storage
permission (Android settings -> Apps -> Emacs -> Permissions)."
  :type 'directory :group 'beatmacs)

(defcustom beatmacs-player-extra-args nil
  "Alist of (PLAYER . ARGS) appended to that player's arguments.
Example: \\='((mpv \"--ao=opensles\"))."
  :type '(alist :key-type symbol :value-type (repeat string))
  :group 'beatmacs)

(defconst beatmacs-players
  '((mpv :program "mpv" :loops t
         :args ("--no-video" "--really-quiet" "--loop-file=inf"))
    (ffplay :program "ffplay" :loops t
            :args ("-nodisp" "-loglevel" "quiet" "-loop" "0"))
    (sox :program "play" :args ("-q"))
    (paplay :program "paplay")
    (aplay :program "aplay" :args ("-q"))
    (android-open :function beatmacs--android-open
                  :available beatmacs--android-p)
    (emacs :function beatmacs--emacs-play
           :available beatmacs--emacs-sound-p))
  "Known players.  Process players have :program/:args/:loops;
others have :function (called with the file) and :available.")

(defvar beatmacs--proc nil "Running player process.")
(defvar beatmacs--loop-file nil "File being looped, or nil when stopped.")

(defun beatmacs--android-p () (eq system-type 'android))

(defun beatmacs--emacs-sound-p ()
  (and (fboundp 'play-sound-internal) (not (beatmacs--android-p))))

(defun beatmacs--exec-path ()
  "`exec-path' plus Termux's bin directory when present."
  (if (file-directory-p beatmacs-termux-bin)
      (append exec-path (list beatmacs-termux-bin))
    exec-path))

(defun beatmacs--find-program (name)
  "Absolute path of NAME, also looking in Termux."
  (let ((exec-path (beatmacs--exec-path)))
    (executable-find name)))

(defun beatmacs-player-available-p (id)
  "Non-nil if player ID can be used here."
  (let ((spec (alist-get id beatmacs-players)))
    (if (plist-get spec :program)
        (beatmacs--find-program (plist-get spec :program))
      (funcall (plist-get spec :available)))))

(defun beatmacs-player-resolve ()
  "The player that will actually be used (a symbol), or nil."
  (if (eq beatmacs-player 'auto)
      (cl-loop for (id . _) in beatmacs-players
               when (beatmacs-player-available-p id) return id)
    beatmacs-player))

(defun beatmacs-player-playing-p ()
  "Non-nil while a loop is playing."
  (and beatmacs--loop-file t))

(defun beatmacs-player-stop ()
  "Stop playback."
  (interactive)
  (setq beatmacs--loop-file nil)
  (when (process-live-p beatmacs--proc)
    (delete-process beatmacs--proc))
  (setq beatmacs--proc nil))

(defvar beatmacs-player-stopped-hook nil
  "Hook run when playback ends on its own (e.g. the player failed).")

(defun beatmacs--sentinel (proc _event)
  "Restart non-looping players while the loop is active."
  (when (and (eq proc beatmacs--proc)
             (not (process-live-p proc))
             beatmacs--loop-file)
    (if (and (zerop (process-exit-status proc))
             (not (plist-get (process-get proc 'spec) :loops)))
        (beatmacs--start (process-get proc 'player) beatmacs--loop-file)
      (setq beatmacs--loop-file nil beatmacs--proc nil)
      (message "beatmacs: %s exited with status %d (no audio device?)"
               (process-get proc 'player) (process-exit-status proc))
      (run-hooks 'beatmacs-player-stopped-hook))))

(defun beatmacs--start (id file)
  "Start player ID on FILE."
  (let* ((spec (alist-get id beatmacs-players))
         (prog (beatmacs--find-program (plist-get spec :program)))
         (process-environment
          (if (file-directory-p beatmacs-termux-bin)
              (cons (format "PATH=%s:%s" beatmacs-termux-bin (getenv "PATH"))
                    process-environment)
            process-environment))
         (proc (apply #'start-process "beatmacs-player" nil prog
                      (append (plist-get spec :args)
                              (alist-get id beatmacs-player-extra-args)
                              (list (expand-file-name file))))))
    (process-put proc 'spec spec)
    (process-put proc 'player id)
    (set-process-query-on-exit-flag proc nil)
    (set-process-sentinel proc #'beatmacs--sentinel)
    (setq beatmacs--proc proc)))

(defun beatmacs-player-play (file)
  "Loop FILE with the configured player.  Stops any previous loop."
  (beatmacs-player-stop)
  (let* ((id (or (beatmacs-player-resolve)
                 (user-error "beatmacs: no audio player found (install mpv)")))
         (spec (alist-get id beatmacs-players)))
    (if (plist-get spec :function)
        (funcall (plist-get spec :function) file)
      (setq beatmacs--loop-file file)
      (beatmacs--start id file))
    id))

(defun beatmacs--android-open (file)
  "Open FILE in Android's media player app (one-shot, no loop)."
  (let ((shared (expand-file-name (file-name-nondirectory file)
                                  beatmacs-android-shared-directory)))
    (condition-case err
        (progn
          (make-directory beatmacs-android-shared-directory t)
          (copy-file file shared t)
          (android-browse-url (concat "file://" shared)))
      (error
       (message "beatmacs: could not open player (%s). File is at %s"
                (error-message-string err) shared)))))

(defun beatmacs--emacs-play (file)
  "Play FILE once with Emacs' built-in sound support (blocking)."
  (message "beatmacs: playing once with play-sound-file (blocks Emacs)...")
  (play-sound-file file))

(provide 'beatmacs-player)
;;; beatmacs-player.el ends here
