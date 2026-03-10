;;; emacspeak-telega.el --- Speech-enable Telega -*- lexical-binding: t; -*-
;; $Id$
;; Author: ChatGPT (Codex)
;; Description: Speech extension for the Telega Telegram client
;; Keywords: Emacspeak, Audio Desktop, Telega

;;;   Copyright:
;; Copyright (C) 2024, T. V. Raman
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
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139,
;; USA.
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Commentary:
;; Basic Telega support:
;; - Speak the chat or message we land on when navigating with n/p.
;; - Strip emojis and compress whitespace for succinct speech.

;;; Code:

;;   Required modules:

(eval-when-compile (require 'cl-lib))
(cl-declaim (optimize  (safety 0) (speed 3)))
(require 'subr-x)
(require 'emacspeak-preamble)
(require 'telega nil 'no-error)

(declare-function telega-chat-title "telega-chat" (chat &optional no-badges))
(declare-function telega-msg-content-text "telega-msg" (msg &optional with-speech-recognition-p))
(declare-function telega-msg-sender "telega-msg" (tl-obj))
(declare-function telega-msg-sender-title "telega-msg" (msg-sender &rest args))

;;; Customization:

(defgroup emacspeak-telega nil
  "Speech extensions for Telega."
  :group 'emacspeak)

(defcustom emacspeak-telega-summary-max 180
  "Maximum number of characters to speak from a Telega item.
Set to nil to speak the full cleaned string."
  :type '(choice (const :tag "Unlimited" nil) integer)
  :group 'emacspeak-telega)

;;; Helpers:

(defconst emacspeak-telega--emoji-regexp
  ;; Broad emoji and symbol ranges plus variation selectors/ZWJ.
  (rx (or
       (regexp "[\U0001F000-\U0001FAFF]")
       (regexp "[\U0001F300-\U0001F5FF]")
       (regexp "[\U0001F600-\U0001F64F]")
       (regexp "[\U0001F680-\U0001F6FF]")
       (regexp "[\U0001F900-\U0001F9FF]")
       (regexp "[\u2600-\u27BF]")
       (regexp "[\uFE0E\uFE0F\u200D]"))))

(defun emacspeak-telega--clean-string (text)
  "Strip emoji-like glyphs from TEXT and compress whitespace."
  (when text
    (let* ((raw (substring-no-properties (format "%s" text)))
           (no-emoji (replace-regexp-in-string
                      emacspeak-telega--emoji-regexp " " raw))
           (spaced (replace-regexp-in-string "[ \t\n\r\f\v\u00a0]+" " " no-emoji))
           ;; Drop leading avatar-initial + bracketed chat title noise.
           (no-leading (replace-regexp-in-string
                        "\\`[[:alnum:]]\\[[^]]+\\]\\s*" "" spaced))
           ;; Drop leading @nick> prompt fragments if present.
           (no-prompt (replace-regexp-in-string "\\`@[[:graph:]]+>\\s*" "" no-leading))
           ;; Drop trailing timestamps like “ 12:34”.
           (no-time (replace-regexp-in-string "\\s+[0-2]?[0-9]:[0-5][0-9]\\'" "" no-prompt)))
      (string-trim no-time))))

(defun emacspeak-telega--truncate (text &optional max)
  "Truncate TEXT using MAX or `emacspeak-telega-summary-max'.
If MAX is `:full' or nil when `emacspeak-telega-summary-max' is nil,
no truncation is applied."
  (let ((limit (cond
                ((eq max :full) nil)
                ((numberp max) max)
                (t emacspeak-telega-summary-max))))
    (if (or (null limit)
            (<= (length text) limit))
        text
      (concat (substring text 0 (- limit 3)) "..."))))

(defun emacspeak-telega--button-text (button)
  "Return cleaned text for BUTTON."
  (emacspeak-telega--clean-string
   (buffer-substring-no-properties (button-start button) (button-end button))))

(defun emacspeak-telega--join (parts)
  "Join PARTS with commas, dropping empty entries."
  (mapconcat #'identity
             (cl-remove-if (lambda (s) (or (null s) (string-empty-p s)))
                           (nreverse parts))
             ", "))

(defun emacspeak-telega--chat-summary (button)
  "Summarize chat BUTTON for speech."
  (let* ((chat (button-get button :value))
         (title (when (and chat (fboundp 'telega-chat-title))
                  (emacspeak-telega--clean-string
                   (telega-chat-title chat t))))
         (line (emacspeak-telega--clean-string
                (emacspeak-telega--button-text button)))
         (unread (when chat (plist-get chat :unread_count)))
         (mentions (when chat (plist-get chat :unread_mention_count)))
         (bits nil))
    (when title (push title bits))
    (when (and unread (> unread 0))
      (push (format "%d unread" unread) bits))
    (when (and mentions (> mentions 0))
      (push (format "%d mentions" mentions) bits))
    ;; Only use the rendered line if it adds new info and is not a
    ;; bracketed/duplicated title.
    (when (and line
               (not (string-empty-p line))
               (not (and title (string-prefix-p title line)))
               (not (string-match-p "\\`[[:alnum:]]\\[[^]]+\\]" line))
               (not (string-prefix-p "@" line)))
      (push line bits))
    (when bits
      (emacspeak-telega--truncate (emacspeak-telega--join bits) :full))))

(defun emacspeak-telega--msg-summary (button)
  "Summarize message BUTTON for speech."
  (let* ((msg (button-get button :value))
         (sender (when (and msg (fboundp 'telega-msg-sender))
                   (telega-msg-sender msg)))
         (sender-name
          (when (and sender (fboundp 'telega-msg-sender-title))
            (emacspeak-telega--clean-string
             (telega-msg-sender-title sender :with-brackets-p nil :with-badges-p nil))))
         (msg-text
          (or (when (and msg (fboundp 'telega-msg-content-text))
                (emacspeak-telega--clean-string
                 (telega-msg-content-text msg 'with-speech-recognition-p)))
              (emacspeak-telega--button-text button)))
         (bits nil))
    (when sender-name (push sender-name bits))
    (when msg-text (push msg-text bits))
    (when bits
      (emacspeak-telega--truncate (emacspeak-telega--join bits)))))

(defun emacspeak-telega--msg-nav-summary (button)
  "Navigation summary for message BUTTON including time."
  (let* ((msg (button-get button :value))
         (sender (when (and msg (fboundp 'telega-msg-sender))
                   (telega-msg-sender msg)))
         (sender-name
          (when (and sender (fboundp 'telega-msg-sender-title))
            (emacspeak-telega--clean-string
             (telega-msg-sender-title sender :with-brackets-p nil :with-badges-p nil))))
         (msg-text
          (or (when (and msg (fboundp 'telega-msg-content-text))
                (emacspeak-telega--clean-string
                 (telega-msg-content-text msg 'with-speech-recognition-p)))
              (emacspeak-telega--button-text button)))
         (time-str
          (when-let ((ts (plist-get msg :date)))
            (format-time-string "%R" (seconds-to-time ts))))
         (bits nil))
    (when sender-name (push sender-name bits))
    (when msg-text (push msg-text bits))
    (when time-str (push time-str bits))
    (when bits
      (emacspeak-telega--truncate (emacspeak-telega--join bits)))))

(defun emacspeak-telega--speak-button (&optional button)
  "Speak BUTTON or button at point."
  (let* ((b (or button (button-at (point))))
         (summary
          (when b
            (or (and (eq (button-type b) 'telega-chat)
                     (emacspeak-telega--chat-summary b))
                (and (eq (button-type b) 'telega-msg)
                     (emacspeak-telega--msg-summary b))
                (emacspeak-telega--truncate (emacspeak-telega--button-text b))))))
    (if (and b summary (not (string-empty-p summary)))
        (progn
          (when (fboundp 'emacspeak-auditory-icon)
            (emacspeak-auditory-icon 'select-object))
          (dtk-speak summary))
      (emacspeak-speak-line))))

;;; Advice interactive navigation:

(eval-after-load "telega"
  #'(lambda ()
      (defadvice telega-button-forward (after emacspeak pre act comp)
        "Speak target after moving forward."
        (when (ems-interactive-p)
          (emacspeak-telega--speak-button)))
      (defadvice telega-button-backward (after emacspeak pre act comp)
        "Speak target after moving backward."
        (when (ems-interactive-p)
          (emacspeak-telega--speak-button)))
      (defadvice telega-msg-next (after emacspeak pre act comp)
        "Speak message summary (author, text, time) after moving to next message."
        (when (ems-interactive-p)
          (let ((emacspeak-speak-messages nil))
            (when-let ((btn (button-at (point))))
              (when (eq (button-type btn) 'telega-msg)
                (when-let ((summary (emacspeak-telega--msg-nav-summary btn)))
                  (when (fboundp 'emacspeak-auditory-icon)
                    (emacspeak-auditory-icon 'select-object))
                  (dtk-speak summary)))))))
      (defadvice telega-msg-previous (after emacspeak pre act comp)
        "Speak message summary (author, text, time) after moving to previous message."
        (when (ems-interactive-p)
          (let ((emacspeak-speak-messages nil))
            (when-let ((btn (button-at (point))))
              (when (eq (button-type btn) 'telega-msg)
                (when-let ((summary (emacspeak-telega--msg-nav-summary btn)))
                  (when (fboundp 'emacspeak-auditory-icon)
                    (emacspeak-auditory-icon 'select-object))
                  (dtk-speak summary)))))))
      (defadvice telega-button--help-echo (around emacspeak pre act comp)
        "Silence help-echo messages to avoid double speech on navigation."
        (let ((emacspeak-speak-messages nil))
          ad-do-it))))

(provide 'emacspeak-telega)
;;; emacspeak-telega.el ends here
