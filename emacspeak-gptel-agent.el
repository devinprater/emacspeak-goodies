;;; emacspeak-gptel-agent.el --- Speech-enable gptel-agent  -*- lexical-binding: t; -*-

;; Author: T. V. Raman / Emacspeak contributors
;; Keywords: convenience, comm

;;; Commentary:
;; Speech-enable gptel-agent's tool-confirmation UI.
;;
;; gptel (and gptel-agent) renders tool calls as overlays with the text
;; property `gptel-tool'.  Sighted users see the prompt inserted into the
;; buffer, but Emacspeak currently doesn't announce when tool approval is
;; requested.
;;
;; This module announces:
;; - when tool calls are displayed (approval requested)
;; - when tool calls are accepted/rejected.

;;; Code:

(require 'emacspeak-preamble)

(with-eval-after-load "gptel"
  (with-eval-after-load "gptel-agent-tools"

    (defun emacspeak-gptel-agent--folded-todo-at-point-p ()
      "Return non-nil if point is on a folded (hidden) gptel-agent todo overlay."
      (pcase-let ((`(,_ . ,ov)
                   (or (get-char-property-and-overlay (point) 'gptel-agent--todos)
                       (get-char-property-and-overlay
                        (previous-single-char-property-change
                         (point) 'gptel-agent--todos nil (point-min))
                        'gptel-agent--todos))))
        (and (overlayp ov)
             (overlay-get ov 'gptel-agent--todos)
             ;; When folded, after-string is nil; when visible, after-string is set.
             (not (overlay-get ov 'after-string)))))

    (defvar emacspeak-gptel-agent--todo-fold-pos nil
      "Last position where we played the folded-todo indicator.")

    ;; NOTE: Don't call `ems-interactive-p' from here.  Emacspeak rewrites
    ;; it lexically inside its own `defadvice' expansions; calling it as a
    ;; function signals "Unexpected call".
    (defun emacspeak-gptel-agent--maybe-indicate-folded-todo ()
      "Play ellipses when entering a folded todo list overlay."
      (when (emacspeak-gptel-agent--folded-todo-at-point-p)
        (unless (equal emacspeak-gptel-agent--todo-fold-pos (point))
          (setq emacspeak-gptel-agent--todo-fold-pos (point))
          (emacspeak-icon 'ellipses))))

    ;; Don't advise next-line/previous-line directly here: Emacspeak already
    ;; advises them and relies on its own `ems-interactive-p' macroexpansion.
    ;; Instead, hook the folded-todo indicator into line-speech itself.
    (defadvice emacspeak-speak-line (before emacspeak-gptel-agent-todo-fold pre act comp)
      "Play ellipses when speaking a folded todo list overlay line."
      (when (emacspeak-gptel-agent--folded-todo-at-point-p)
        (emacspeak-icon 'ellipses)))
    ;; Announce todo list updates produced by gptel-agent.
    (defun emacspeak-gptel-agent--summarize-todos (todos)
      "Return a short summary string for TODOS.

TODOS is a list of plists with keys :content, :activeForm and :status."
      (condition-case _err
          (let ((in-progress
                 (cl-loop for todo in todos
                          when (equal (plist-get todo :status) "in_progress")
                          return (or (plist-get todo :activeForm)
                                     (plist-get todo :content))))
                (pending
                 (cl-loop for todo in todos
                          count (equal (plist-get todo :status) "pending")))
                (completed
                 (cl-loop for todo in todos
                          count (equal (plist-get todo :status) "completed"))))
            (concat
             (and in-progress (format "Task: %s. " in-progress))
             (format "%d pending, %d completed" pending completed)))
        (error "Task list updated")))

    (defadvice gptel-agent--write-todo (after emacspeak pre act comp)
      "Speak when gptel-agent updates its task list overlay."
      (when (and (boundp 'emacspeak-speak-messages)
                 emacspeak-speak-messages)
        (let ((todos (ad-get-arg 0)))
          (when (consp todos)
            (emacspeak-icon 'progress)
            (dtk-notify (emacspeak-gptel-agent--summarize-todos todos))))))

    (defadvice gptel-agent-toggle-todos (after emacspeak pre act comp)
      "Auditory icon when toggling todo list overlay display."
      (when (ems-interactive-p)
        (pcase-let ((`(,_ . ,ov)
                     (or (get-char-property-and-overlay (point) 'gptel-agent--todos)
                         (get-char-property-and-overlay
                          (previous-single-char-property-change
                           (point) 'gptel-agent--todos nil (point-min))
                          'gptel-agent--todos))))
          ;; If overlay has after-string, list is visible. If nil, it is hidden.
          (if (overlay-get ov 'after-string)
              (emacspeak-icon 'open-object)
            (emacspeak-icon 'ellipses))))))
  (defun emacspeak-gptel--summarize-tool-calls (tool-calls)
    "Return a short summary string for TOOL-CALLS.

TOOL-CALLS is a list of (TOOL-SPEC ARG-VALUES CALLBACK)."
    (condition-case _err
        (mapconcat
         (lambda (call)
           (let* ((tool-spec (nth 0 call))
                  (args (nth 1 call))
                  (name (and tool-spec (fboundp 'gptel-tool-name)
                             (gptel-tool-name tool-spec))))
             (cond
              ((and name (listp args))
               (format "%s %s" name
                       (mapconcat
                        (lambda (a) (format "%S" a))
                        (seq-take args (min 3 (length args)))
                        " ")))
              (name name)
              (t "tool"))))
         tool-calls ", ")
      (error "tool call")))

  (defun emacspeak-gptel--tool-approval-message (tool-calls)
    "Return a full minibuffer-friendly approval prompt for TOOL-CALLS."
    (format "%s wants to run: %s"
            (condition-case _err
                (gptel-backend-name gptel-backend)
              (error "GPTEL"))
            (emacspeak-gptel--summarize-tool-calls tool-calls)))

  ;; Announce tool approval request (buffer overlay path).
  (defadvice gptel--display-tool-calls (after emacspeak pre act comp)
    "Speak when tool calls are being presented for confirmation."
    (when (and (boundp 'emacspeak-speak-messages)
               emacspeak-speak-messages)
      (let ((calls (ad-get-arg 0)))
        (when (consp calls)
          (emacspeak-icon 'ask-short-question)
          (dtk-notify (emacspeak-gptel--tool-approval-message calls))))))

  ;; Status updates: keep it minimal.
  ;; - Speak Waiting...
  ;; - Speak Calling tool...
  ;; - Speak Run tools? (pending approvals)
  ;; - Speak errors
  ;; Don't speak Ready.

  (defadvice gptel--update-wait (after emacspeak pre act comp)
    "Speak gptel wait status."
    (when (and (ems-interactive-p)
               (boundp 'emacspeak-speak-messages)
               emacspeak-speak-messages)
      (emacspeak-icon 'progress)
      (dtk-notify "Waiting")))

  (defadvice gptel--update-tool-call (after emacspeak pre act comp)
    "Speak when gptel is calling a tool."
    (when (and (ems-interactive-p)
               (boundp 'emacspeak-speak-messages)
               emacspeak-speak-messages)
      (emacspeak-icon 'working)
      (dtk-notify "Calling tool")))

  (defadvice gptel--update-tool-ask (after emacspeak pre act comp)
    "Speak when gptel has pending tool calls requiring confirmation."
    (when (and (ems-interactive-p)
               (boundp 'emacspeak-speak-messages)
               emacspeak-speak-messages)
      (emacspeak-icon 'ask-short-question)
      (dtk-notify "Run tools")))

  ;; Minibuffer prompt path used by gptel when USE-MINIBUFFER is non-nil.
  ;; map-y-or-n-p only shows the (y,n,!,...) boilerplate on completion; it
  ;; does not necessarily speak the full prompt.  So we speak it up-front.
  (defadvice map-y-or-n-p (before emacspeak-gptel-tool-approval pre act comp)
    "Speak the full gptel tool approval prompt when gptel uses map-y-or-n-p."
    (condition-case _err
        (when (and (boundp 'gptel--fsm-last)
                   (consp gptel--fsm-last))
          (let* ((prompter (ad-get-arg 0))
                 (actor (ad-get-arg 1))
                 (list (ad-get-arg 2))
                 (help (ad-get-arg 3))
                 (maybe-first
                  (condition-case _err2
                      (if (functionp list) (funcall list) (car-safe list))
                    (error nil)))
                 (prompt
                  (and maybe-first
                       (cond
                        ((stringp prompter) (format prompter maybe-first))
                        ((functionp prompter) (funcall prompter maybe-first))))))
            (when (and (stringp prompt)
                       (string-match-p " wants to run " prompt))
              (emacspeak-icon 'ask-short-question)
              (dtk-notify prompt))))
      (error nil)))

  ;; Announce accept/reject.
  (defadvice gptel--accept-tool-calls (after emacspeak pre act comp)
    "Speak when tool calls are accepted."
    (when (ems-interactive-p)
      (emacspeak-icon 'yes-answer)
      (dtk-notify "Running tool calls")))

  (defadvice gptel--reject-tool-calls (after emacspeak pre act comp)
    "Speak when tool calls are rejected."
    (when (ems-interactive-p)
      (emacspeak-icon 'no-answer)
      (dtk-notify "Tool calls rejected")))

  (defadvice gptel--handle-error (after emacspeak pre act comp)
    "Speak gptel request errors."
    (when (and (ems-interactive-p)
               (boundp 'emacspeak-speak-messages)
               emacspeak-speak-messages)
      (emacspeak-icon 'warn-user)
      (dtk-notify "GPTEL error")))

  (defun emacspeak-gptel--tool-result-summary (tool-results)
    "Return a short summary for TOOL-RESULTS.

TOOL-RESULTS is ((tool args result) ...)."
    (condition-case _err
        (mapconcat
         (lambda (tr)
           (let* ((tool (nth 0 tr))
                  (result (nth 2 tr))
                  (name (and tool (fboundp 'gptel-tool-name)
                             (gptel-tool-name tool)))
                  (line (and (stringp result)
                             (car (split-string result "\n" t)))))
             (cond
              ((and name line)
               (format "%s: %s" name (truncate-string-to-width line 60 nil nil t)))
              (name name)
              (t "tool"))))
         tool-results ", ")
      (error "tool result")))

  (defadvice gptel--display-tool-results (after emacspeak pre act comp)
    "Speak after tool results are inserted."
    (when (and (ems-interactive-p)
               (boundp 'emacspeak-speak-messages)
               emacspeak-speak-messages)
      (let ((results (ad-get-arg 0)))
        (when (consp results)
          (emacspeak-icon 'task-done)
          (dtk-notify
           (format "Tool result. %s" (emacspeak-gptel--tool-result-summary results)))))))

  ;; Auto-speak non-tool responses in gptel buffers.
  ;; Speak only *assistant text*, i.e. response text inserted with `gptel'
  ;; text property.  This skips tool-call prompts and other UI.
  (defadvice gptel--handle-post-insert (after emacspeak pre act comp)
    "Speak assistant output after it is inserted."
    (when (and (boundp 'gptel-mode) gptel-mode
               (boundp 'emacspeak-speak-messages) emacspeak-speak-messages)
      (let* ((info (gptel-fsm-info (ad-get-arg 0)))
             (start-marker (plist-get info :position))
             (tracking-marker (or (plist-get info :tracking-marker)
                                  start-marker))
             (start (and (markerp start-marker) (marker-position start-marker)))
             (end (and (markerp tracking-marker) (marker-position tracking-marker))))
        (when (and (integerp start) (integerp end) (< start end))
          (save-excursion
            (goto-char start)
            ;; Speak only text marked as gptel response, skipping tool-call UI.
            (let ((pos start) rbeg rend)
              ;; Find first `gptel' text property with value `response'.
              (while (and (< pos end)
                          (not (eq (get-text-property pos 'gptel) 'response)))
                (setq pos (next-single-property-change pos 'gptel nil end)))
              (when (and (< pos end)
                         (eq (get-text-property pos 'gptel) 'response))
                (setq rbeg pos)
                ;; Extend across adjacent runs of gptel=response up to END.
                (while (and (< pos end)
                            (eq (get-text-property pos 'gptel) 'response))
                  (setq pos (next-single-property-change pos 'gptel nil end)))
                (setq rend pos)
                (when (< rbeg rend)
                  (emacspeak-icon 'item)
                  (condition-case _err
                      (emacspeak-speak-region rbeg rend)
                    (error nil)))))))))))


(provide 'emacspeak-gptel-agent)
;;; emacspeak-gptel-agent.el ends here
