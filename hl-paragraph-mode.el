;;; hl-paragraph-mode.el --- Minor mode for highlighting paragraphs -*- lexical-binding: t -*-

;; Copyright (C) 2024 Niklas Vågstedt

;; Author:     Niklas Vågstedt <niklas@niklas.zone>
;; Created:    23 Feb 2014
;; Version:    1.0.0
;; Keywords:   convenience, faces, accessibility
;; URL:        https://github.com/niklasva/hl-paragraph-mode

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
;; A minor mode for highlighting the current paragraph under the
;; cursor. Similar to the built-in hl-line mode.
;;

;;; Code:

(defface hl-paragraph-face
  '((t (:inherit hl-line)))
  "Face for highlighting paragraphs."
  :group 'hl-paragraph)

(defvar hl-paragraph-overlay nil
  "Overlay to highlight the current paragraph.")

(defvar hl-paragraph-highlight-entire-line t
  "Variable to toggle between highlighting lines or characters inside a paragraph.
  Non-nil means highlight whole lines.")

(defun hl-paragraph-highlight ()
  "Highlight the paragraph under the cursor."
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
      (setq hl-paragraph-overlay (make-overlay start end))
      (overlay-put hl-paragraph-overlay 'face 'hl-paragraph-face))))

(defun hl-paragraph-unhighlight ()
  "Remove highlighting from paragraph."
  (when hl-paragraph-overlay
    (delete-overlay hl-paragraph-overlay)
    (setq hl-paragraph-overlay nil)))

(define-minor-mode hl-paragraph-mode
  "Toggle highlighting of the current paragraph under the cursor."
  :init-value nil
  :global nil
  (if hl-paragraph-mode
      (progn
        (set-face-attribute 'hl-paragraph-face nil :extend hl-paragraph-highlight-entire-line)
        (add-hook 'post-command-hook 'hl-paragraph-highlight nil t)
        (add-hook 'pre-command-hook 'hl-paragraph-unhighlight nil t))
    (remove-hook 'post-command-hook 'hl-paragraph-highlight t)
    (remove-hook 'pre-command-hook 'hl-paragraph-unhighlight t)))

(define-globalized-minor-mode global-hl-paragraph-mode
  hl-paragraph-mode
  hl-paragraph-mode)


(provide 'hl-paragraph-mode)

;;; hl-paragraph-mode.el ends here
