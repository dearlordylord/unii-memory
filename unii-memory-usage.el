;;; unii-memory--usage.el --- Session unii-memory-work accounting -*- lexical-binding: t -*-
;; Copyright (C) 2026 Igor Loskutov
;; Author: Igor Loskutov <igor.loskutoff@gmail.com>
;; Maintainer: Igor Loskutov <igor.loskutoff@gmail.com>
;; Assisted-by: Codex:gpt-6-1-sol
;; URL: https://github.com/dearlordylord/unii-memory
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: data, tools
;; SPDX-License-Identifier: MIT

;;; Commentary:
;; Persistent journal and binary summary tree implementing the UNII design.
;; See README.org for the public API, backend contract and platform requirements.

;;; Code:

(require 'cl-lib)
(defvar unii-memory--usage--epochs (make-hash-table :test #'eq :weakness 'key))
(defvar unii-memory--usage--ledgers (make-hash-table :test #'eq :weakness 'key))
(cl-defstruct (unii-memory--usage (:constructor unii-memory--usage-create))
  (started 0) (input 0) (output 0) (input-observed 0) (output-observed 0)
  (attempts (make-hash-table :test #'equal)))
;; Cache telemetry is external so existing live ledger identities stay valid.
(defvar unii-memory--usage--cache (make-hash-table :test #'eq :weakness 'key))
(cl-defstruct (unii-memory--cache (:constructor unii-memory--cache-create))
  (cached-input 0) (cached-input-observed 0) (cache-write-input 0) (cache-write-input-observed 0))
(defun unii-memory--usage-cache (ledger)
  (or (gethash ledger unii-memory--usage--cache)
      (puthash ledger (unii-memory--cache-create) unii-memory--usage--cache)))
(defun unii-memory--usage-cached-input (ledger)
  (unii-memory--cache-cached-input (unii-memory--usage-cache ledger)))
(gv-define-setter unii-memory--usage-cached-input (value ledger)
  `(setf (unii-memory--cache-cached-input (unii-memory--usage-cache ,ledger)) ,value))
(defun unii-memory--usage-cached-input-observed (ledger)
  (unii-memory--cache-cached-input-observed (unii-memory--usage-cache ledger)))
(gv-define-setter unii-memory--usage-cached-input-observed (value ledger)
  `(setf (unii-memory--cache-cached-input-observed (unii-memory--usage-cache ,ledger)) ,value))
(defun unii-memory--usage-cache-write-input (ledger)
  (unii-memory--cache-cache-write-input (unii-memory--usage-cache ledger)))
(gv-define-setter unii-memory--usage-cache-write-input (value ledger)
  `(setf (unii-memory--cache-cache-write-input (unii-memory--usage-cache ,ledger)) ,value))
(defun unii-memory--usage-cache-write-input-observed (ledger)
  (unii-memory--cache-cache-write-input-observed (unii-memory--usage-cache ledger)))
(gv-define-setter unii-memory--usage-cache-write-input-observed (value ledger)
  `(setf (unii-memory--cache-cache-write-input-observed (unii-memory--usage-cache ,ledger)) ,value))
(defun unii-memory--usage-start (ledger id)
  "Register one attempt exactly once."
  (unless (gethash id (unii-memory--usage-attempts ledger))
    (cl-incf (unii-memory--usage-started ledger))
    (puthash id (list :started t) (unii-memory--usage-attempts ledger))))
(defun unii-memory--usage-observe (ledger id counters)
  "Replace explicitly reported counters; duplicates do not increase sums."
  (let ((entry (gethash id (unii-memory--usage-attempts ledger))))
    (when entry
      (dolist (key '(:input :output :cached-input :cache-write-input))
        (let ((value (plist-get counters key)) (old (plist-get entry key)))
          (when (natnump value)
            (pcase key
              (:input
               (cl-incf (unii-memory--usage-input ledger) (- value (or old 0)))
               (unless old (cl-incf (unii-memory--usage-input-observed ledger))))
              (:output
               (cl-incf (unii-memory--usage-output ledger) (- value (or old 0)))
               (unless old (cl-incf (unii-memory--usage-output-observed ledger))))
              (:cached-input
               (cl-incf (unii-memory--usage-cached-input ledger) (- value (or old 0)))
               (unless old (cl-incf (unii-memory--usage-cached-input-observed ledger))))
              (:cache-write-input
               (cl-incf (unii-memory--usage-cache-write-input ledger) (- value (or old 0)))
               (unless old (cl-incf (unii-memory--usage-cache-write-input-observed ledger)))))
            (setq entry (plist-put entry key value)))))
      (puthash id entry (unii-memory--usage-attempts ledger)))))
(defun unii-memory--usage-project (ledger)
  "Project known subtotals with separate coverage; missing is not zero."
  (append (list :started (unii-memory--usage-started ledger)
        :input (unii-memory--usage-input ledger)
        :output (unii-memory--usage-output ledger)
        :inputObserved (unii-memory--usage-input-observed ledger)
        :outputObserved (unii-memory--usage-output-observed ledger)
        :cachedInput (unii-memory--usage-cached-input ledger)
        :cachedInputObserved (unii-memory--usage-cached-input-observed ledger)
        :cacheWriteInput (unii-memory--usage-cache-write-input ledger)
        :cacheWriteInputObserved (unii-memory--usage-cache-write-input-observed ledger))
          (gethash ledger unii-memory--usage--epochs)))
(defun unii-memory--usage-retire (ledger id)
  "Release late-update state without reducing accumulated totals."
  (remhash id (unii-memory--usage-attempts ledger)))

(defun unii-memory--usage-observe-total (ledger attempt state counters)
  "Account absolute thread COUNTERS once across all retries sharing STATE.
Coverage counts attempts with a reported counter, not per-attempt attribution.
Older and duplicate cumulative snapshots cannot add expenditure again."
  (when (gethash attempt (unii-memory--usage-attempts ledger))
    (dolist (key '(:input :output :cached-input :cache-write-input))
      (let* ((value (plist-get counters key))
             (totals (plist-get state :totals))
             (previous (or (plist-get totals key) 0)))
        (when (natnump value)
          ;; Zero marks telemetry coverage without adding a per-request delta.
          (unii-memory--usage-observe ledger attempt (list key 0))
          (when (> value previous)
            (let ((delta (- value previous)))
              (pcase key
                (:input (cl-incf (unii-memory--usage-input ledger) delta))
                (:output (cl-incf (unii-memory--usage-output ledger) delta))
                (:cached-input (cl-incf (unii-memory--usage-cached-input ledger) delta))
                (:cache-write-input (cl-incf (unii-memory--usage-cache-write-input ledger) delta))))
            (setf (plist-get state :totals) (plist-put totals key value))))))))

(provide 'unii-memory-usage)
