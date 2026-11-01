;;; beatmacs.el --- Generate beats for rapping, in Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Georg

;; Author: Georg
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: multimedia, music
;; URL: https://github.com/galberding/beatmacs

;; This file is not part of GNU Emacs.

;;; Commentary:

;; beatmacs generates drum & bass loops (Dubstep, Drum & Bass,
;; Rap/R&B) with a pure Emacs Lisp synthesizer and plays them in a
;; loop so you can rap or sing over them.  The control panel works
;; with keyboard and with touch on Emacs for Android.
;;
;;   M-x beatmacs
;;
;; Files:
;;   beatmacs-synth.el   oscillators, filters, instruments, mixer, WAV
;;   beatmacs-core.el    track model, sequencing, registries, backends
;;   beatmacs-styles.el  drums/bass layers, genres, learning notes
;;   beatmacs-player.el  external player detection (mpv, Termux, ...)
;;   beatmacs-ui.el      the panel, options and pages
;;
;; See README.org for the extension API.

;;; Code:

(require 'beatmacs-ui)

(provide 'beatmacs)
;;; beatmacs.el ends here
