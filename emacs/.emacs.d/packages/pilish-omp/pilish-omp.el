;;; pilish-omp.el --- Drive Pilish with the omp CLI -*- lexical-binding: t; -*-

;; Author: local overlay
;; Package-Requires: ((emacs "29.1") (pilish "3.0"))
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Pilish fronts the pi coding-agent CLI over its JSON-RPC stdio
;; protocol.  omp is a superset of pi on the same protocol, so Pilish
;; can drive it with two adjustments:
;;
;;   1. Launch omp instead of pi, without pi's --approve trust flag
;;      (omp rejects it and decides project trust itself).
;;   2. Rename omp's `session_settled' event to pi's `agent_settled',
;;      which Pilish's status machine keys on to leave the streaming
;;      state and release queued follow-ups.
;;
;; Load this file after `pilish'.  It edits no Pilish sources, so MELPA
;; upgrades apply unchanged.

;;; Code:

(require 'pilish)

(defgroup pilish-omp nil
  "omp backend for Pilish."
  :group 'pilish
  :prefix "pilish-omp-")

(defcustom pilish-omp-event-aliases
  '(("session_settled" . "agent_settled"))
  "Inbound RPC event `:type' rewrites, as (OMP-TYPE . PI-TYPE) pairs.
omp renames a few pi protocol events."
  :type '(repeat (cons (string :tag "omp type")
                       (string :tag "pi type")))
  :group 'pilish-omp)

(defun pilish-omp--filter-args (args)
  "Rewrite aliased event types in dispatch ARGS, (PROC JSON)."
  (pcase-let ((`(,proc ,json) args))
    (let ((alias
           (cdr (assoc (plist-get json :type) pilish-omp-event-aliases))))
      (list proc (if alias
                     (plist-put (copy-sequence json) :type alias)
                   json)))))

(advice-add 'pilish--dispatch-response :filter-args #'pilish-omp--filter-args)

(defcustom pilish-omp-env-strip
  '("\\`SUPACODE_" "\\`ZMX_" "\\`OMPCODE=")
  "Regexps for `process-environment' entries removed for the omp RPC child.
Harness socket variables (e.g. supacode) make a nested omp attach to an
existing live agent session instead of starting its own."
  :type '(repeat regexp)
  :group 'pilish-omp)

(defun pilish-omp--around-start-process (orig directory)
  "Call ORIG `pilish--start-process' with harness entries stripped from the env."
  (let ((process-environment
         (seq-remove (lambda (entry)
                       (seq-find (lambda (re) (string-match-p re entry))
                                 pilish-omp-env-strip))
                     process-environment)))
    (funcall orig directory)))

(advice-add 'pilish--start-process :around #'pilish-omp--around-start-process)

(setq pilish-executable '("omp")
      pilish-project-trust-policy 'default)

;; omp's agent dir; Pilish's session browser scans sessions under it.
(setenv "PI_CODING_AGENT_DIR" (expand-file-name "~/.omp/agent"))

(provide 'pilish-omp)
;;; pilish-omp.el ends here
