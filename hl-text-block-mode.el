;;; hl-text-block-mode.el --- Minor mode for highlighting text blocks -*- lexical-binding: t -*-

;; Copyright (C) 2024 Niklas Vågstedt

;; Author:     Niklas Vågstedt <niklas@niklas.zone>
;; Created:    23 Feb 2014
;; Version:    1.0.0
;; Keywords:   convenience, faces, accessibility
;; URL:        https://github.com/niklasva/hl-text-block-mode

;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; This file is not part of GNU Emacs.
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; For a full copy of the GNU General Public License
;; see <https://www.gnu.org/licenses/>.
;;

;;; Commentary:
;;
;; A minor mode for highlighting the current text block under the
;; cursor. Similar to the built-in hl-line mode.
;;

;;; Code:

(defface hl-text-block-face
  '((t (:inherit hl-line)))
  "Face for highlighting text blocks."
  :group 'hl-text-block)

(defvar hl-text-block-overlay nil
  "Overlay to highlight the current text block.")

(defvar hl-text-block-highlight-entire-line t
  "Variable to toggle between highlighting lines or characters inside a text block.
  Non-nil means highlight whole lines.")

(defun hl-text-block-highlight ()
  "Highlight the text block under the cursor."
  (when (and (not (minibufferp))
             (not (looking-at-p "[[:space:]]*$")))
    (let ((start (save-excursion
                   (progn
                     (backward-paragraph)
                     (forward-line)
                     (point))))
          (end (save-excursion
                 (progn
                   (forward-paragraph)
                   (point)))))
      (setq hl-text-block-overlay (make-overlay start end))
      (overlay-put hl-text-block-overlay 'face 'hl-text-block-face))))

(defun hl-text-block-unhighlight ()
  "Remove highlighting from the text block."
  (when hl-text-block-overlay
    (delete-overlay hl-text-block-overlay)
    (setq hl-text-block-overlay nil)))

(define-minor-mode hl-text-block-mode
  "Toggle highlighting of the current text block under the cursor."
  :init-value nil
  :lighter " hl-text-block"
  :global nil
  (if hl-text-block-mode
      (progn
        (set-face-attribute 'hl-text-block-face nil :extend hl-text-block-highlight-entire-line)
        (add-hook 'post-command-hook 'hl-text-block-highlight nil t)
        (add-hook 'pre-command-hook 'hl-text-block-unhighlight nil t))
    (remove-hook 'post-command-hook 'hl-text-block-highlight t)
    (remove-hook 'pre-command-hook 'hl-text-block-unhighlight t)))

(define-globalized-minor-mode global-hl-text-block-mode
  hl-text-block-mode
  hl-text-block-mode)


(provide 'hl-text-block-mode)

;;; hl-text-block-mode.el ends here
