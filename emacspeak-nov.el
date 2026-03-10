;;; emacspeak-nov.el --- Speech-enable NOV  -*- lexical-binding: t; -*-
;; $Author: tv.raman.tv $
;; Description:  Speech-enable NOV An Emacs Interface to nov
;; Keywords: Emacspeak,  Audio Desktop nov
;;;   LCD Archive entry:

;; LCD Archive Entry:
;; emacspeak| T. V. Raman |tv.raman.tv@gmail.com
;; A speech interface to Emacs |
;; 
;;  $Revision: 4532 $ |
;; Location https://github.com/tvraman/emacspeak
;; 

;;;   Copyright:
;; Copyright (C) 1995 -- 2007, 2011, T. V. Raman
;; Copyright (c) 1994, 1995 by Digital Equipment Corporation.
;; All Rights Reserved.
;; 
;; This file is not part of GNU Emacs, but the same permissions apply.
;; 
;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;; 
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNNOV FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; Commentary:
;; NOV == Yet Another EPub Reader 
;; Package nov.el is an alternative to Emacspeak's built-in EPub
;; reader.
;; This module speech-enables nov.el
;; In addition, opening an epub using nov results in
;; directory-specific settings being loaded from file
;; @var{emacspeak-speak-directory-settings} ---
;;  That file can set book-specific settings such as speech-rate and
;; punctuation-mode among others.

;;; Code:

;;   Required modules:

(eval-when-compile (require 'cl-lib))
(cl-declaim  (optimize  (safety 0) (speed 3)))
(require 'emacspeak-preamble)

;;; Helpers:

(defun emacspeak-nov-link-text-at-point ()
  "Return visible text for link at point."
  (let* ((button (button-at (point)))
         (label
          (when button
            (string-trim (button-label button)))))
    (cond
     ((and label (> (length label) 0)) label)
     (t (string-trim (buffer-substring-no-properties
                      (line-beginning-position)
                      (line-end-position)))))))

(defun emacspeak-nov-next-link ()
  "Move to next NOV link and speak its label."
  (interactive)
  (shr-next-link)
  (emacspeak-icon 'button)
  (dtk-speak (emacspeak-nov-link-text-at-point)))

(defun emacspeak-nov-previous-link ()
  "Move to previous NOV link and speak its label."
  (interactive)
  (shr-previous-link)
  (emacspeak-icon 'button)
  (dtk-speak (emacspeak-nov-link-text-at-point)))

;;;  Interactive Commands:

(cl-loop
 for f in
 '(shr-next-link shr-previous-link)
 do
 (eval
  `(defadvice ,f (around emacspeak-nov pre act comp)
     "In NOV, speak link text rather than the href target."
     (cond
      ((and (eq major-mode 'nov-mode) (ems-interactive-p))
       (ems-with-messages-silenced ad-do-it)
       (emacspeak-icon 'button)
       (dtk-speak (emacspeak-nov-link-text-at-point)))
      (t ad-do-it)))))

(cl-loop
 for f in 
 '(
   nov-browse-url
   nov-display-metadata
   nov-goto-toc
   nov-next-document
   nov-previous-document
   )
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "speak."
     (when (ems-interactive-p)
       (emacspeak-icon 'open-object)
       (emacspeak-speak-buffer)))))

(cl-loop
 for f in
 '(nov-scroll-up  nov-scroll-down)
 do
 (eval
  `(defadvice ,f (after emacspeak pre act comp)
     "Speak the next screenful."
     (when (ems-interactive-p)
       (emacspeak-icon 'scroll)
       (dtk-speak (emacspeak-get-window-contents))))))

;;; Mode Hook:

(defun emacspeak-nov-mode-hook ()
  "Load directory-specific speech settings."
  (cl-declare (special emacspeak-speak-directory-settings))
  (local-set-key (kbd "TAB") #'emacspeak-nov-next-link)
  (local-set-key (kbd "<backtab>") #'emacspeak-nov-previous-link)
  (local-set-key (kbd "S-TAB") #'emacspeak-nov-previous-link)
  (emacspeak-speak-load-directory-settings default-directory))

(add-hook 'nov-mode-hook #'emacspeak-nov-mode-hook)

(provide 'emacspeak-nov)
;;;  end of file
