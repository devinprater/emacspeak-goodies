;;; emacspeak-mastodon.el --- Speech-enable Mastodon -*- lexical-binding: t; -*-
;; $Id$
;; Author: ChatGPT (Codex)
;; Description: Speech extensions for mastodon.el
;; Keywords: Emacspeak, Audio Desktop, Mastodon

;;;   Copyright:
;; Copyright (C) 2026, T. V. Raman
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
;; Speech-enables mastodon.el timelines and compose buffers.
;; - Speak a readable summary of the current status when navigating.
;; - Speak action results (favourite, boost, bookmark).
;; - Speak when a compose buffer opens or closes.

;;; Code:

(eval-when-compile (require 'cl-lib))
(cl-declaim (optimize (safety 0) (speed 3)))
(require 'subr-x)
(require 'emacspeak-preamble)
(require 'mastodon nil 'no-error)
(require 'mastodon-tl nil 'no-error)
(require 'dom nil 'no-error)
(require 'shr nil 'no-error)

(declare-function mastodon-tl--find-property-range "mastodon-tl"
                  (property start-point &optional search-backwards))
(declare-function mastodon-tl--relative-time-description "mastodon-tl"
                  (timestamp &optional current-time))
(declare-function mastodon-tl--property "mastodon-tl"
                  (prop &optional no-move backward))

;;; Customization:

(defgroup emacspeak-mastodon nil
  "Speech extensions for mastodon.el."
  :group 'emacspeak)

(defcustom emacspeak-mastodon-summary-max 260
  "Maximum number of characters to speak from a Mastodon item.
Set to nil to speak the full cleaned string."
  :type '(choice (const :tag "Unlimited" nil) integer)
  :group 'emacspeak-mastodon)

(defcustom emacspeak-mastodon-alt-text-max 140
  "Maximum number of characters to speak of media descriptions."
  :type 'integer
  :group 'emacspeak-mastodon)

(defcustom emacspeak-mastodon-speak-alt-text t
  "Non-nil means speak media descriptions (alt text) when available."
  :type 'boolean
  :group 'emacspeak-mastodon)

(defcustom emacspeak-mastodon-include-content-warning t
  "Non-nil means include content warnings in spoken summaries."
  :type 'boolean
  :group 'emacspeak-mastodon)

(defcustom emacspeak-mastodon-include-boost-info nil
  "Non-nil means include 'boosted by' information in summaries."
  :type 'boolean
  :group 'emacspeak-mastodon)

(defcustom emacspeak-mastodon-include-handle nil
  "Non-nil means include @handle in spoken author names."
  :type 'boolean
  :group 'emacspeak-mastodon)

(defcustom emacspeak-mastodon-include-notification-type nil
  "Non-nil means prefix summaries with notification type."
  :type 'boolean
  :group 'emacspeak-mastodon)

(defcustom emacspeak-mastodon-include-notification-actors t
  "Non-nil means announce who performed a notification action."
  :type 'boolean
  :group 'emacspeak-mastodon)

;;; Helpers:

;; NOTE: Avoid stripping wide Unicode ranges here; on some builds those
;; regexes match too broadly and nuke real text. We only remove known
;; format/variant codepoints in `emacspeak-mastodon--clean-string'.
(defconst emacspeak-mastodon--emoji-regexp nil)

(defun emacspeak-mastodon--json-false-p (value)
  "Return non-nil if VALUE represents JSON false or null."
  (or (eq value :json-false)
      (eq value :null)))

(defun emacspeak-mastodon--alistp (value)
  "Return non-nil if VALUE looks like an alist."
  (and (listp value)
       (or (null value) (consp (car value)))))

(defun emacspeak-mastodon--json-value (value)
  "Return VALUE or nil if it represents JSON false."
  (if (emacspeak-mastodon--json-false-p value) nil value))

(defun emacspeak-mastodon--clean-string (text)
  "Clean TEXT for speech by trimming, removing emoji, and collapsing space."
  (let ((text (emacspeak-mastodon--json-value text)))
    (when text
      (let* ((raw (substring-no-properties (format "%s" text)))
             (no-bidi (replace-regexp-in-string "[\u2066-\u2069]" " " raw))
             ;; Remove only joiners/variation selectors, not full emoji ranges.
             (no-joins (replace-regexp-in-string "[\u200D\uFE0E\uFE0F]" " " no-bidi))
             (no-urls (replace-regexp-in-string
                       "\\bhttps?://[^[:space:]]+"
                       " link " no-joins))
             (collapsed (replace-regexp-in-string
                         "[ \t\n\r\f\v\u00a0]+" " " no-urls))
             (trimmed (string-trim collapsed))
             (raw-trim (string-trim raw)))
        ;; If cleaning stripped all meaningful chars, fall back to raw.
        (when (and (not (string-match-p "[[:alnum:]]" trimmed))
                   (string-match-p "[[:alnum:]]" raw-trim))
          (setq trimmed raw-trim))
        (unless (or (string-empty-p trimmed)
                    (string= trimmed "nil")
                    (string= trimmed "null")
                    (string= trimmed ":null")
                    (string= trimmed "json-false")
                    (string= trimmed ":json-false"))
          trimmed)))))

(defun emacspeak-mastodon--truncate (text &optional max)
  "Truncate TEXT using MAX or `emacspeak-mastodon-summary-max'."
  (let ((limit (cond
                ((eq max :full) nil)
                ((numberp max) max)
                (t emacspeak-mastodon-summary-max))))
    (if (or (null limit)
            (<= (length text) limit))
        text
      (concat (substring text 0 (- limit 3)) "..."))))

(defun emacspeak-mastodon--dom-text (node)
  "Return concatenated text for DOM NODE, skipping invisible spans."
  (cond
   ((stringp node) node)
   ((not (consp node)) "")
   (t
    (let* ((tag (ignore-errors (dom-tag node)))
           (class (ignore-errors (dom-attr node 'class))))
      (cond
       ((memq tag '(script style)) "")
       ((and (stringp class)
             (string-match-p "\\binvisible\\b" class))
        "")
       ((eq tag 'br) "\n")
       (t
        (mapconcat #'emacspeak-mastodon--dom-text
                   (ignore-errors (dom-children node)) "")))))))

(defun emacspeak-mastodon--html-to-text (html)
  "Convert HTML to plain text using libxml if available."
  (when (and html (fboundp 'libxml-parse-html-region))
    (with-temp-buffer
      (insert "<div>" html "</div>")
      (let ((dom (libxml-parse-html-region (point-min) (point-max))))
        (emacspeak-mastodon--dom-text dom)))))

(defun emacspeak-mastodon--shr-render-text (html)
  "Render HTML to plain text using shr."
  (when (and html (fboundp 'shr-render-region))
    (with-temp-buffer
      (insert html)
      (let ((shr-use-fonts nil)
            (shr-width 0))
        (shr-render-region (point-min) (point-max)))
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun emacspeak-mastodon--html-unescape (text)
  "Unescape common HTML entities in TEXT."
  (when text
    (let ((table '(("&nbsp;" . " ")
                   ("&amp;" . "&")
                   ("&lt;" . "<")
                   ("&gt;" . ">")
                   ("&quot;" . "\"")
                   ("&#39;" . "'"))))
      (dolist (pair table)
        (setq text (replace-regexp-in-string (regexp-quote (car pair))
                                             (cdr pair) text t t)))
      ;; numeric entities
      (setq text
            (replace-regexp-in-string
             "&#\\([0-9]+\\);"
             (lambda (m)
               (let ((n (string-to-number (match-string 1 m))))
                 (string (if (and (> n 0) (< n #x110000)) n ?\s))))
             text t))
      (setq text
            (replace-regexp-in-string
             "&#x\\([0-9a-fA-F]+\\);"
             (lambda (m)
               (let ((n (string-to-number (match-string 1 m) 16)))
                 (string (if (and (> n 0) (< n #x110000)) n ?\s))))
             text t))
      text)))

(defun emacspeak-mastodon--strip-html (html)
  "Strip HTML tags and return text."
  (when html
    (let* ((s (replace-regexp-in-string "<br\\s-*/?>" "\n" html t t))
           (s (replace-regexp-in-string "</p>" "\n" s t t))
           (s (replace-regexp-in-string "<[^>]+>" " " s))
           (s (emacspeak-mastodon--html-unescape s)))
      s)))

(defun emacspeak-mastodon--valid-status-p (value)
  "Return non-nil if VALUE looks like a status JSON alist."
  (and (emacspeak-mastodon--alistp value)
       (or (alist-get 'content value)
           (alist-get 'account value))))

(defun emacspeak-mastodon--base-status (status)
  "Return base status, preferring reblog if it is a valid status."
  (let ((reblog (alist-get 'reblog status)))
    (if (emacspeak-mastodon--valid-status-p reblog)
        reblog
      status)))

(defun emacspeak-mastodon--render-text (html &optional toot)
  "Render HTML as plain text and clean it for speech."
  (let ((html (emacspeak-mastodon--json-value html)))
    (when (and html (not (string-empty-p (format "%s" html))))
      (let ((rendered
             (or (emacspeak-mastodon--shr-render-text html)
                 (emacspeak-mastodon--html-to-text html)
                 (when (fboundp 'mastodon-tl--render-text)
                   (mastodon-tl--render-text html toot))
                 (when (fboundp 'mastodon-tl--remove-html)
                   (mastodon-tl--remove-html html))
                 html)))
        (when (or (null rendered) (string-empty-p (format "%s" rendered)))
          (setq rendered (emacspeak-mastodon--strip-html html)))
        (when (and (stringp rendered)
                   (text-property-not-all 0 (length rendered)
                                          'display nil rendered))
          (setq rendered
                (emacspeak-mastodon--display-string-from-propertized
                 rendered)))
        (emacspeak-mastodon--clean-string rendered)))))

(defun emacspeak-mastodon--placeholder-p (text)
  "Return non-nil if TEXT is a placeholder or noise string."
  (and text
       (stringp text)
       (let* ((trimmed (string-trim text))
              (stripped (replace-regexp-in-string "[^[:alnum:]:-]" "" trimmed))
              (low (downcase stripped)))
         (member low '("nil" "null" ":null" "json-false" ":json-false")))))

(defun emacspeak-mastodon--speakable-p (text)
  "Return non-nil if TEXT is meaningful for speech."
  (and text
       (not (string-empty-p text))
       (not (emacspeak-mastodon--placeholder-p text))
       (string-match-p "[[:alnum:]]" text)))

(defun emacspeak-mastodon--display-string-from-propertized (text)
  "Return display text for propertized TEXT."
  (when (stringp text)
    (let ((pos 0)
          (len (length text))
          (out nil))
      (while (< pos len)
        (let* ((before (get-text-property pos 'before-string text))
               (after (get-text-property pos 'after-string text))
               (display (get-text-property pos 'display text))
               (invis (get-text-property pos 'invisible text))
               (next-display (next-single-property-change pos 'display text len))
               (next-invis (next-single-property-change pos 'invisible text len))
               (next (min next-display next-invis)))
          (when (stringp before) (push before out))
          (cond
           ((and invis (not (eq invis 'emacspeak))) nil)
           ((stringp display) (push display out))
           ((and (consp display) (stringp (car display)))
            (push (car display) out))
           (t
            (let ((ch (aref text pos)))
              (push (string ch) out))))
          (when (stringp after) (push after out))
          (setq pos next)))
      (apply #'concat (nreverse out)))))

(defun emacspeak-mastodon--display-string-range (beg end)
  "Return display-aware text for buffer range BEG to END."
  (let ((pos beg)
        (out nil))
    (while (< pos end)
      (let* ((before (get-text-property pos 'before-string))
             (after (get-text-property pos 'after-string))
             (display (get-text-property pos 'display))
             (invis (get-text-property pos 'invisible))
             (next-display (next-single-property-change pos 'display nil end))
             (next-invis (next-single-property-change pos 'invisible nil end))
             (next (min next-display next-invis)))
        (when (stringp before) (push before out))
        (cond
         ((and invis (not (eq invis 'emacspeak))) nil)
         ((stringp display) (push display out))
         ((and (consp display) (stringp (car display)))
          (push (car display) out))
         (t
          (let ((ch (char-after pos)))
            (when ch (push (string ch) out)))))
        (when (stringp after) (push after out))
        (setq pos next)))
    (apply #'concat (nreverse out))))

(defun emacspeak-mastodon--range-text (property &optional backward)
  "Return cleaned text for PROPERTY range around point."
  (when (fboundp 'mastodon-tl--find-property-range)
    (when-let ((range (mastodon-tl--find-property-range
                       property (point) backward)))
      (let* ((beg (car range))
             (end (cdr range))
             (display-text (emacspeak-mastodon--display-string-range beg end))
             (raw (or display-text
                      (buffer-substring-no-properties beg end))))
        (emacspeak-mastodon--clean-string raw)))))

(defun emacspeak-mastodon--byline-text ()
  "Return cleaned byline text for toot at point."
  (emacspeak-mastodon--range-text 'byline t))

(defun emacspeak-mastodon--body-text ()
  "Return cleaned toot body text for toot at point."
  (emacspeak-mastodon--range-text 'toot-body t))

(defun emacspeak-mastodon--author-from-byline (byline)
  "Extract an author string from BYLINE."
  (when (and byline (not (string-empty-p byline)))
    (let* ((first-line (car (split-string byline "\n")))
           (trimmed (emacspeak-mastodon--clean-string first-line)))
      (when (and trimmed (not (string-empty-p trimmed)))
        ;; Drop trailing timestamp-like phrases.
        (let* ((no-time
                (replace-regexp-in-string
                 "\\s-+\\([0-9]+\\s-+\\(sec\\|secs\\|second\\|seconds\\|min\\|mins\\|minute\\|minutes\\|hour\\|hours\\|day\\|days\\)\\b.*\\)\\'" "" trimmed))
               (no-just-now
                (replace-regexp-in-string
                 "\\s-+just now\\b.*\\'" "" no-time))
               (no-yesterday
                (replace-regexp-in-string
                 "\\s-+yesterday\\b.*\\'" "" no-just-now)))
          (unless (or (emacspeak-mastodon--placeholder-p no-yesterday)
                      (string-empty-p no-yesterday))
            no-yesterday))))))

(defun emacspeak-mastodon--stats-line-p (line)
  "Return non-nil if LINE looks like the stats line."
  (and line
       (string-match-p "[⭐🔁💬]" line)
       (not (string-match-p "[[:alpha:]]" line))))

(defun emacspeak-mastodon--byline-first-line (byline)
  "Return the first meaningful line from BYLINE."
  (when byline
    (cl-loop for line in (split-string byline "\n")
             for cleaned = (emacspeak-mastodon--clean-string line)
             when (and (emacspeak-mastodon--speakable-p cleaned)
                       (not (emacspeak-mastodon--stats-line-p cleaned)))
             return cleaned)))

(defun emacspeak-mastodon--buffer-summary ()
  "Return a summary derived from buffer text properties."
  (let* ((byline (emacspeak-mastodon--byline-text))
         (body (emacspeak-mastodon--body-text))
         (byline-line (emacspeak-mastodon--byline-first-line byline))
         (parts nil))
    (when (emacspeak-mastodon--speakable-p byline-line)
      (push byline-line parts))
    (when (emacspeak-mastodon--speakable-p body)
      (push (format "Content: %s" body) parts))
    (when parts
      (emacspeak-mastodon--truncate
       (emacspeak-mastodon--join parts)))))

(defun emacspeak-mastodon--join (parts &optional sep)
  "Join PARTS with SEP, dropping empty or placeholder entries."
  (mapconcat #'identity
             (cl-remove-if (lambda (s)
                             (or (null s)
                                 (string-empty-p s)
                                 (emacspeak-mastodon--placeholder-p s)))
                           (nreverse parts))
             (or sep ". ")))

(defun emacspeak-mastodon--count (value)
  "Return numeric VALUE or 0."
  (cond
   ((numberp value) value)
   ((stringp value) (string-to-number value))
   (t 0)))

(defun emacspeak-mastodon--handle-p (handle)
  "Return non-nil if HANDLE looks like a usable account handle."
  (and handle
       (stringp handle)
       (string-match-p "[[:alnum:]]" handle)))

(defun emacspeak-mastodon--account-name (account)
  "Return a readable account string from ACCOUNT json."
  (when account
    (let* ((display-raw (alist-get 'display_name account))
           (acct-raw (alist-get 'acct account))
           (username-raw (alist-get 'username account))
           (display (or (emacspeak-mastodon--clean-string display-raw)
                        (string-trim (format "%s" display-raw))))
           (acct (or (emacspeak-mastodon--clean-string acct-raw)
                     (string-trim (format "%s" acct-raw))))
           (username (or (emacspeak-mastodon--clean-string username-raw)
                         (string-trim (format "%s" username-raw)))))
      (cond
       ((and display (not (string-empty-p display)))
        (if (and emacspeak-mastodon-include-handle
                 (emacspeak-mastodon--handle-p acct))
            (format "%s @%s" display acct)
          display))
       ((and display (not (string-empty-p display)))
        display)
       ((and emacspeak-mastodon-include-handle
             (emacspeak-mastodon--handle-p acct))
        (format "@%s" acct))
       ((and emacspeak-mastodon-include-handle
             (emacspeak-mastodon--handle-p username))
        (format "@%s" username))
       (t nil)))))

(defun emacspeak-mastodon--visibility-label (visibility)
  "Return a human readable label for VISIBILITY."
  (when visibility
    (pcase visibility
      ("public" "Public")
      ("unlisted" "Unlisted")
      ("private" "Followers only")
      ("direct" "Direct")
      (_ visibility))))

(defun emacspeak-mastodon--timestamp (iso)
  "Return a human readable timestamp for ISO string."
  (let ((iso (emacspeak-mastodon--json-value iso)))
    (when iso
      (let ((time (condition-case nil
                      (date-to-time iso)
                    (error nil))))
        (when time
          (if (fboundp 'mastodon-tl--relative-time-description)
              (emacspeak-mastodon--expand-time-units
               (mastodon-tl--relative-time-description time))
            (format-time-string "%Y-%m-%d %H:%M" time)))))))

(defun emacspeak-mastodon--expand-time-units (text)
  "Expand abbreviated time units in TEXT."
  (when text
    (let ((s text))
      (setq s (replace-regexp-in-string "\\bsecs?\\b" "seconds" s t))
      (setq s (replace-regexp-in-string "\\bmins?\\b" "minutes" s t))
      (setq s (replace-regexp-in-string "\\bhrs?\\b" "hours" s t))
      (setq s (replace-regexp-in-string "\\bsec\\b" "second" s t))
      (setq s (replace-regexp-in-string "\\bmin\\b" "minute" s t))
      (setq s (replace-regexp-in-string "\\bhr\\b" "hour" s t))
      (setq s (replace-regexp-in-string "\\b1 seconds\\b" "1 second" s t))
      (setq s (replace-regexp-in-string "\\b1 minutes\\b" "1 minute" s t))
      (setq s (replace-regexp-in-string "\\b1 hours\\b" "1 hour" s t))
      s)))

(defun emacspeak-mastodon--sanitize-summary (text)
  "Clean up summary TEXT for speech."
  (when text
    (let ((s text))
      (setq s (replace-regexp-in-string
               "\\`nil[[:space:]]*\\([\\.,:;—-]+\\s-*\\)?" "" s))
      (setq s (replace-regexp-in-string "[ \t\n\r\f\v\u00a0]+" " " s))
      (string-trim s))))

(defun emacspeak-mastodon--media-type-label (type count)
  "Return a label for media TYPE, pluralized for COUNT."
  (let* ((base (pcase type
                 ("image" "image")
                 ("video" "video")
                 ("gifv" "animated gif")
                 ("audio" "audio")
                 (_ (or type "media")))))
    (if (= count 1) base (concat base "s"))))

(defun emacspeak-mastodon--media-summary (toot)
  "Return a summary of media attachments in TOOT."
  (let ((attachments (alist-get 'media_attachments toot)))
    (when (and attachments (listp attachments))
      (let ((counts (make-hash-table :test 'equal))
            (descriptions nil))
        (dolist (att attachments)
          (let ((type (alist-get 'type att)))
            (when type
              (puthash type (1+ (gethash type counts 0)) counts)))
          (when emacspeak-mastodon-speak-alt-text
            (let ((desc (emacspeak-mastodon--clean-string
                         (alist-get 'description att))))
              (when (and desc (not (string-empty-p desc)))
                (push desc descriptions)))))
        (let (parts)
          (maphash
           (lambda (type count)
             (push (format "%s %s"
                           count
                           (emacspeak-mastodon--media-type-label type count))
                   parts))
           counts)
          (setq parts (nreverse parts))
          (let ((summary (when parts
                           (format "Media: %s"
                                   (mapconcat #'identity parts ", ")))))
            (when (and summary (eq t (alist-get 'sensitive toot)))
              (setq summary (concat summary ". Sensitive")))
            (when (and summary emacspeak-mastodon-speak-alt-text descriptions)
              (setq summary
                    (concat summary
                            ". Alt: "
                            (emacspeak-mastodon--truncate
                             (mapconcat #'identity
                                        (nreverse descriptions) "; ")
                             emacspeak-mastodon-alt-text-max))))
            summary))))))

(defun emacspeak-mastodon--poll-summary (toot)
  "Return a summary for the poll in TOOT."
  (let ((poll (alist-get 'poll toot)))
    (when (and poll (listp poll))
      (let* ((options (alist-get 'options poll))
             (expired (alist-get 'expired poll))
             (expires-at (alist-get 'expires_at poll))
             (option-texts
              (when (and options (listp options))
                (mapcar
                 (lambda (opt)
                   (let* ((title (emacspeak-mastodon--clean-string
                                  (alist-get 'title opt)))
                          (votes (alist-get 'votes_count opt)))
                     (if (and votes (numberp votes))
                         (format "%s (%s)" title votes)
                       title)))
                 options))))
        (let ((parts nil))
          (when option-texts
            (push (format "Poll: %s"
                          (mapconcat #'identity option-texts ", "))
                  parts))
          (when expired
            (push "Poll closed" parts))
          (when (and (not expired) expires-at)
            (when-let ((ts (emacspeak-mastodon--timestamp expires-at)))
              (push (format "Poll ends %s" ts) parts)))
          (when parts
            (emacspeak-mastodon--join parts ", ")))))))

(defun emacspeak-mastodon--stats-summary (toot)
  "Return a summary string of stats in TOOT."
  (let* ((replies (emacspeak-mastodon--count
                   (alist-get 'replies_count toot)))
         (boosts (emacspeak-mastodon--count
                  (alist-get 'reblogs_count toot)))
         (faves (emacspeak-mastodon--count
                 (alist-get 'favourites_count toot)))
         (parts nil))
    (when (> replies 0)
      (push (format "%s replies" replies) parts))
    (when (> boosts 0)
      (push (format "%s boosts" boosts) parts))
    (when (> faves 0)
      (push (format "%s favourites" faves) parts))
    (when parts
      (format "Stats: %s" (emacspeak-mastodon--join parts ", ")))))

(defun emacspeak-mastodon--user-flags (toot)
  "Return a summary of the current user's actions on TOOT."
  (let (parts)
    (when (eq t (alist-get 'favourited toot))
      (push "favourited" parts))
    (when (eq t (alist-get 'reblogged toot))
      (push "boosted" parts))
    (when (eq t (alist-get 'bookmarked toot))
      (push "bookmarked" parts))
    (when parts
      (format "You: %s" (emacspeak-mastodon--join parts ", ")))))

(defun emacspeak-mastodon--status-summary (status)
  "Return a readable summary for STATUS."
  (let* ((boosted (alist-get 'reblog status))
         (base (emacspeak-mastodon--base-status status))
         (booster (and (emacspeak-mastodon--valid-status-p boosted)
                       (alist-get 'account status)))
         (author (emacspeak-mastodon--account-name
                  (alist-get 'account base)))
         (byline (emacspeak-mastodon--byline-text))
         (visibility (emacspeak-mastodon--visibility-label
                      (emacspeak-mastodon--json-value
                       (alist-get 'visibility base))))
         (timestamp (emacspeak-mastodon--timestamp
                     (alist-get 'created_at base)))
         (reply (emacspeak-mastodon--json-value
                 (alist-get 'in_reply_to_id base)))
         (edited (emacspeak-mastodon--json-value
                  (alist-get 'edited_at base)))
         (cw (emacspeak-mastodon--render-text
              (alist-get 'spoiler_text base) base))
         (content (emacspeak-mastodon--render-text
                   (alist-get 'content base) base))
         (content-raw (alist-get 'content base))
         (body (emacspeak-mastodon--body-text))
         (parts nil))
    (unless (emacspeak-mastodon--speakable-p author)
      (setq author (emacspeak-mastodon--author-from-byline byline)))
    (unless (emacspeak-mastodon--speakable-p content)
      (let* ((raw-text (emacspeak-mastodon--html-to-text content-raw))
             (clean-raw (emacspeak-mastodon--clean-string raw-text)))
        (setq content (or clean-raw body ""))))
    (when (and emacspeak-mastodon-include-boost-info booster)
      (when-let ((booster-name (emacspeak-mastodon--account-name booster)))
        (push (format "Boosted by %s" booster-name) parts)))
    (when author (push author parts))
    (when (emacspeak-mastodon--speakable-p cw)
      (when emacspeak-mastodon-include-content-warning
        (setq content (if (emacspeak-mastodon--speakable-p content)
                          (format "CW %s. %s" cw content)
                        (format "CW %s" cw)))))
    (when (emacspeak-mastodon--speakable-p content)
      (push content parts))
    (when timestamp (push timestamp parts))
    (when visibility (push visibility parts))
    (when parts
      (emacspeak-mastodon--truncate
       (emacspeak-mastodon--join parts)))))

(defun emacspeak-mastodon--notification-type ()
  "Return notification type at point as a string, if any."
  (let ((nt (get-text-property (point) 'notification-type)))
    (cond
     ((symbolp nt) (symbol-name nt))
     ((stringp nt) (unless (emacspeak-mastodon--placeholder-p nt) nt))
     (t nil))))

(defun emacspeak-mastodon--notification-actors ()
  "Return a list of actor names for the notification at point."
  (let* ((accounts (get-text-property (point) 'notification-accounts))
         (json (get-text-property (point) 'item-json))
         (from-json (when (and (emacspeak-mastodon--alistp json)
                               (alist-get 'account json))
                      (list (alist-get 'account json)))))
    (setq accounts (or accounts from-json))
    (when (and accounts (listp accounts))
      (cl-remove-if #'null
                    (mapcar #'emacspeak-mastodon--account-name accounts)))))

(defun emacspeak-mastodon--names-equal-p (a b)
  "Return non-nil if names A and B are equal (case-insensitive)."
  (and a b (string= (downcase a) (downcase b))))

(defun emacspeak-mastodon--status-author-name (status)
  "Return author name for STATUS."
  (let* ((base (emacspeak-mastodon--base-status status))
         (account (and (emacspeak-mastodon--alistp base)
                       (alist-get 'account base))))
    (emacspeak-mastodon--account-name account)))

(defun emacspeak-mastodon--reply-target-name (reply-to-user reply-json)
  "Return a readable reply target name."
  (let ((user (when reply-to-user
                (emacspeak-mastodon--clean-string reply-to-user))))
    (or (and user (not (string-empty-p user)) user)
        (when (emacspeak-mastodon--alistp reply-json)
          (or (emacspeak-mastodon--status-author-name reply-json)
              (emacspeak-mastodon--account-name
               (alist-get 'account reply-json)))))))

(defun emacspeak-mastodon--notification-actor-prefix (type)
  "Return a prefix string describing notification TYPE actors."
  (when (and emacspeak-mastodon-include-notification-actors type)
    (let ((actors (emacspeak-mastodon--notification-actors)))
      (when actors
        (let ((names (mapconcat #'identity actors ", ")))
          (pcase type
            ("reblog" (format "Boosted by %s" names))
            ("favourite" (format "Favourited by %s" names))
            ("follow" (format "Followed by %s" names))
            ("follow_request" (format "Follow request from %s" names))
            ("mention" (format "Mention from %s" names))
            ("poll" (format "Poll from %s" names))
            ("status" (format "Posted by %s" names))
            (_ (format "%s: %s" (capitalize type) names))))))))

(defun emacspeak-mastodon--status-from-json (json)
  "Return a status object from JSON, when possible."
  (let ((nested (and json (alist-get 'status json))))
    (cond
     ((emacspeak-mastodon--valid-status-p nested) nested)
     ((emacspeak-mastodon--valid-status-p json) json)
     (t nil))))

(defun emacspeak-mastodon--item-text ()
  "Return a cleaned text range for the current item."
  (when (fboundp 'mastodon-tl--find-property-range)
    (when-let ((range (mastodon-tl--find-property-range 'item-json (point))))
      (emacspeak-mastodon--clean-string
       (buffer-substring-no-properties (car range) (cdr range))))))

(defun emacspeak-mastodon--item-summary ()
  "Return a readable summary for the current item."
  (let* ((item-type (get-text-property (point) 'item-type))
         (json (get-text-property (point) 'item-json))
         (notif-type (emacspeak-mastodon--notification-type)))
    (cond
     ((eq item-type 'toot)
      (let* ((status (emacspeak-mastodon--status-from-json json))
             (prefix (when (and emacspeak-mastodon-include-notification-type
                                notif-type)
                       (capitalize notif-type)))
             (actors (emacspeak-mastodon--notification-actors))
             (author-name (and status (emacspeak-mastodon--status-author-name status)))
             (actor-prefix (emacspeak-mastodon--notification-actor-prefix notif-type)))
        (let ((summary
               (when status
                 (let ((s (emacspeak-mastodon--status-summary status)))
                   (when (emacspeak-mastodon--speakable-p s)
                     (if prefix
                         (format "%s. %s" prefix s)
                       s))))))
          (when (and actor-prefix summary)
            (when (and actors author-name
                       (cl-some (lambda (n) (emacspeak-mastodon--names-equal-p n author-name))
                                actors))
              (setq actor-prefix nil))
            (setq summary (format "%s. %s" actor-prefix summary)))
          (or summary
              (emacspeak-mastodon--buffer-summary)
              (emacspeak-mastodon--item-text)
              (emacspeak-mastodon--clean-string
               (thing-at-point 'line t))))))
     (t
      (or (emacspeak-mastodon--item-text)
          (emacspeak-mastodon--clean-string
           (thing-at-point 'line t)))))))

(defun emacspeak-mastodon-debug-item ()
  "Display debug info for the Mastodon item at point."
  (interactive)
  (let* ((item-type (get-text-property (point) 'item-type))
         (notif-type (emacspeak-mastodon--notification-type))
         (item-id (get-text-property (point) 'item-id))
         (base-item-id (get-text-property (point) 'base-item-id))
         (json (get-text-property (point) 'item-json))
         (status (emacspeak-mastodon--status-from-json json))
         (reblog (and status (alist-get 'reblog status)))
         (base (and status (emacspeak-mastodon--base-status status)))
         (author (and (emacspeak-mastodon--alistp base)
                      (alist-get 'account base)))
         (author-name (and author (emacspeak-mastodon--account-name author)))
         (byline (emacspeak-mastodon--byline-text))
         (body (emacspeak-mastodon--body-text))
         (summary (and status (emacspeak-mastodon--status-summary status)))
         (content-raw (and (emacspeak-mastodon--alistp base)
                           (alist-get 'content base)))
         (content-rendered (and content-raw
                                (emacspeak-mastodon--render-text content-raw base)))
         (buf (get-buffer-create "*emacspeak-mastodon-debug*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "Point: %s\n" (point)))
        (insert (format "Item-type: %S\n" item-type))
        (insert (format "Notification-type: %S\n" notif-type))
        (insert (format "Item-id: %S\n" item-id))
        (insert (format "Base-item-id: %S\n\n" base-item-id))
        (insert "Byline (cleaned):\n")
        (insert (or byline "<nil>"))
        (insert "\n\nBody (cleaned):\n")
        (insert (or body "<nil>"))
        (insert "\n\nAuthor name (computed):\n")
        (insert (or author-name "<nil>"))
        (insert "\n\nAuthor raw fields:\n")
        (when author
          (let ((display-raw (alist-get 'display_name author))
                (acct-raw (alist-get 'acct author))
                (username-raw (alist-get 'username author)))
            (insert (format "display_name raw: %S\n" display-raw))
            (insert (format "acct raw: %S\n" acct-raw))
            (insert (format "username raw: %S\n" username-raw))
            (insert (format "display_name cleaned: %S\n"
                            (emacspeak-mastodon--clean-string display-raw)))
            (insert (format "acct cleaned: %S\n"
                            (emacspeak-mastodon--clean-string acct-raw)))
            (insert (format "username cleaned: %S\n"
                            (emacspeak-mastodon--clean-string username-raw)))))
        (insert "\n\nReblog raw:\n")
        (insert (if reblog (pp-to-string reblog) "<nil>"))
        (insert "\n\nSummary (generated):\n")
        (insert (or summary "<nil>"))
        (insert "\n\nContent raw (length):\n")
        (insert (if content-raw (format "%d" (length content-raw)) "<nil>"))
        (insert "\n\nContent raw (head):\n")
        (insert (if content-raw
                    (substring content-raw 0 (min 200 (length content-raw)))
                  "<nil>"))
        (insert "\n\nContent rendered:\n")
        (insert (or content-rendered "<nil>"))
        (insert "\n\nJSON keys:\n")
        (insert (format "status has account: %s\n"
                        (and (emacspeak-mastodon--alistp status)
                             (not (emacspeak-mastodon--json-false-p
                                   (alist-get 'account status))))))
        (insert (format "status has content: %s\n"
                        (and (emacspeak-mastodon--alistp status)
                             (not (emacspeak-mastodon--json-false-p
                                   (alist-get 'content status))))))
        (insert (format "status has reblog: %s\n"
                        (and (emacspeak-mastodon--alistp status)
                             (not (emacspeak-mastodon--json-false-p
                                   (alist-get 'reblog status))))))
        (insert (format "base has account: %s\n"
                        (and (emacspeak-mastodon--alistp base)
                             (not (emacspeak-mastodon--json-false-p
                                   (alist-get 'account base))))))
        (insert (format "base has content: %s\n"
                        (and (emacspeak-mastodon--alistp base)
                             (not (emacspeak-mastodon--json-false-p
                                   (alist-get 'content base))))))
        (insert (format "created_at: %S\n"
                        (and (emacspeak-mastodon--alistp base)
                             (alist-get 'created_at base))))
        (insert (format "visibility: %S\n"
                        (and (emacspeak-mastodon--alistp base)
                             (alist-get 'visibility base))))
        (insert "\nAuthor raw:\n")
        (insert (if author (pp-to-string author) "<nil>"))
        (goto-char (point-min))
        (view-mode 1)))
    (display-buffer buf)
    (when (fboundp 'emacspeak-auditory-icon)
      (emacspeak-auditory-icon 'open-object))
    (dtk-speak "Mastodon debug buffer ready")))

(defun emacspeak-mastodon--speak-item ()
  "Speak a readable summary of the current Mastodon item."
  (let ((summary (emacspeak-mastodon--item-summary)))
    (setq summary (emacspeak-mastodon--sanitize-summary summary))
    (if (and summary (not (string-empty-p summary)))
        (progn
          (when (fboundp 'emacspeak-auditory-icon)
            (emacspeak-auditory-icon 'select-object))
          (dtk-speak summary))
      (emacspeak-speak-line))))

(defun emacspeak-mastodon--byline-flag (flag)
  "Return FLAG property from the byline at point."
  (when (fboundp 'mastodon-tl--find-property-range)
    (when-let ((range (mastodon-tl--find-property-range 'byline (point) t)))
      (get-text-property (car range) flag))))

(defun emacspeak-mastodon--speak-action (action)
  "Speak ACTION state for current toot."
  (let ((state
         (pcase action
           ('favourite (if (emacspeak-mastodon--byline-flag 'favourited-p)
                           "Favourited" "Unfavourited"))
           ('boost (if (emacspeak-mastodon--byline-flag 'boosted-p)
                       "Boosted" "Unboosted"))
           ('bookmark (if (emacspeak-mastodon--byline-flag 'bookmarked-p)
                          "Bookmarked" "Bookmark removed"))
           (_ (capitalize (symbol-name action))))))
    (when (fboundp 'emacspeak-auditory-icon)
      (emacspeak-auditory-icon 'task-done))
    (dtk-speak state)))

;;; Advice interactive commands:

(eval-after-load
    "mastodon"
  #'(lambda ()
      (when (require 'mastodon-tl nil 'no-error)
        (defadvice mastodon-tl-goto-next-item (after emacspeak pre act comp)
          "Speak the status we land on."
          (when (ems-interactive-p)
            (let ((emacspeak-speak-messages nil))
              (emacspeak-mastodon--speak-item))))
        (defadvice mastodon-tl-goto-prev-item (after emacspeak pre act comp)
          "Speak the status we land on."
          (when (ems-interactive-p)
            (let ((emacspeak-speak-messages nil))
              (emacspeak-mastodon--speak-item))))
        (defadvice mastodon-tl-next-tab-item (after emacspeak pre act comp)
          "Speak the item we land on."
          (when (ems-interactive-p)
            (let ((emacspeak-speak-messages nil))
              (emacspeak-mastodon--speak-item))))
        (defadvice mastodon-tl-previous-tab-item (after emacspeak pre act comp)
          "Speak the item we land on."
          (when (ems-interactive-p)
            (let ((emacspeak-speak-messages nil))
              (emacspeak-mastodon--speak-item))))
        (defadvice mastodon-tl-toggle-fold (after emacspeak pre act comp)
          "Speak fold status."
          (when (ems-interactive-p)
            (let ((folded (mastodon-tl--property 'toot-folded :no-move)))
              (when (fboundp 'emacspeak-auditory-icon)
                (emacspeak-auditory-icon 'button))
              (dtk-speak (if folded "Toot folded" "Toot unfolded")))))
        (defadvice mastodon-tl-toggle-spoiler-text-in-toot (after emacspeak pre act comp)
          "Speak spoiler toggle."
          (when (ems-interactive-p)
            (when (fboundp 'emacspeak-auditory-icon)
              (emacspeak-auditory-icon 'button))
            (dtk-speak "Toggled content warning"))))
      (when (require 'mastodon-toot nil 'no-error)
        (defadvice mastodon-toot-toggle-favourite (after emacspeak pre act comp)
          "Speak favourite/unfavourite action."
          (when (ems-interactive-p)
            (let ((emacspeak-speak-messages nil))
              (emacspeak-mastodon--speak-action 'favourite))))
        (defadvice mastodon-toot-toggle-boost (after emacspeak pre act comp)
          "Speak boost/unboost action."
          (when (ems-interactive-p)
            (let ((emacspeak-speak-messages nil))
              (emacspeak-mastodon--speak-action 'boost))))
        (defadvice mastodon-toot-toggle-bookmark (after emacspeak pre act comp)
          "Speak bookmark toggle action."
          (when (ems-interactive-p)
            (let ((emacspeak-speak-messages nil))
              (emacspeak-mastodon--speak-action 'bookmark))))
        (defadvice mastodon-toot--compose-buffer (after emacspeak pre act comp)
          "Speak when a compose buffer opens."
          (let* ((reply-to-user (ad-get-arg 0))
                 (reply-to-id (ad-get-arg 1))
                 (reply-json (ad-get-arg 2))
                 (edit (ad-get-arg 4))
                 (label (cond
                         (edit "Edit toot")
                         (reply-to-id "Reply to toot")
                         (t "New toot"))))
            (when (fboundp 'emacspeak-auditory-icon)
              (emacspeak-auditory-icon 'open-object))
            (let ((target (emacspeak-mastodon--reply-target-name
                           reply-to-user reply-json)))
              (if (and reply-to-id target)
                  (dtk-speak (format "%s. Replying to %s" label target))
                (dtk-speak label))))))
        (defadvice mastodon-toot-send (after emacspeak pre act comp)
          "Speak after sending a toot."
          (when (ems-interactive-p)
            (when (fboundp 'emacspeak-auditory-icon)
              (emacspeak-auditory-icon 'task-done))
            (dtk-speak "Toot sent")))
        (defadvice mastodon-toot-cancel (after emacspeak pre act comp)
          "Speak after canceling compose."
          (when (ems-interactive-p)
            (when (fboundp 'emacspeak-auditory-icon)
              (emacspeak-auditory-icon 'close-object))
            (dtk-speak "Toot canceled")))))

(provide 'emacspeak-mastodon)
;;; emacspeak-mastodon.el ends here
