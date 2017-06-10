;;; mtg.el --- Build decks for Magic: The Gathering  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Mathieu Marques

;; Author: Mathieu Marques <mathieumarques78@gmail.com>
;; Created: June 10, 2017
;; Homepage: https://github.com/angrybacon/mtg
;; Keywords: convenience, games

;; This program is free software. You can redistribute it and/or modify it under
;; the terms of the Do What The Fuck You Want To Public License, version 2 as
;; published by Sam Hocevar.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.
;;
;; You should have received a copy of the Do What The Fuck You Want To Public
;; License along with this program. If not, see http://www.wtfpl.net/.

;;; Commentary:

;; This package provides modes to facilitates building Magic: The Gathering card
;; decks.
;;
;; See http://www.mtgjson.com/.

;;; Code:

(require 'format-spec)

(defgroup mtg nil
  "Build decks for Magic: The Gathering."
  :group 'games
  :prefix "mtg-")

(defcustom mtg-buffer-name-format "*%m: %n*"
  "The format string used to name MTG buffers.
The following %-sequences are supported:
`%m' The name of the major-mode, minus the `-mode' suffix.
`%n' The name of the deck."
  :group 'mtg
  :type 'string)

(defvar mtg-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap used in `mtg-mode' buffers.")

(define-derived-mode mtg-mode special-mode "MTG"
  "Parent major mode from which MTG major modes inherit."
  :group 'mtg
  (buffer-disable-undo)
  (setq buffer-read-only t))

(defun mtg-generate-buffer-name (mode deck)
  "Generate a buffer name based on `mtg-buffer-name-format'."
  (format-spec mtg-buffer-name-format `((?m . ,(symbol-name mode))
                                        (?n . ,deck))))

;;;###autoload
(defun mtg-deck-create ()
  "Open a new MTG buffer to create a new deck."
  (interactive)
  (let* ((mode 'mtg-mode)
         (deck "Lantern")
         (buffer (get-buffer-create (mtg-generate-buffer-name mode deck))))
    (switch-to-buffer buffer)
    (funcall mode)))

(provide 'mtg)
;;; mtg.el ends here
