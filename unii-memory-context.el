;;; unii-memory-context.el --- Session-only request snapshots -*- lexical-binding: t -*-
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
(defvar unii-memory-context-terminal-limit 100)
(defvar unii-memory-context-byte-limit (* 32 1024 1024))
(cl-defstruct (unii-memory-context-store (:constructor unii-memory-context-store-create))
  (session (format "%s-%s-%s" (emacs-pid) (float-time) (random most-positive-fixnum))) (sequence 0) entries)
(defun unii-memory-context-copy (value)
  "Copy mutable payload containers, including strings and hash tables."
  (cond ((stringp value) (copy-sequence value))
        ((consp value) (cons (unii-memory-context-copy (car value))
                            (unii-memory-context-copy (cdr value))))
        ((vectorp value) (apply #'vector (mapcar #'unii-memory-context-copy value)))
        ((hash-table-p value)
         (let ((copy (make-hash-table :test (hash-table-test value))))
           (maphash (lambda (key item)

                      (puthash (unii-memory-context-copy key) (unii-memory-context-copy item) copy)) value)
           copy))
        (t value)))
(defun unii-memory-context-prune (store)
  "Evict oldest terminal entries; never truncate or evict active input."
  (let ((terminal (cl-remove-if-not (lambda (e) (plist-get e :terminal))
                                    (unii-memory-context-store-entries store))))
    (while (and terminal
                (or (> (length terminal) unii-memory-context-terminal-limit)
                    (> (cl-loop for e in (unii-memory-context-store-entries store)
                                sum (plist-get e :bytes)) unii-memory-context-byte-limit)))
      (let ((oldest (car (last terminal))))
        (setf (unii-memory-context-store-entries store)
              (delq oldest (unii-memory-context-store-entries store)))
        (setq terminal (delq oldest terminal))))))
(defun unii-memory-context-capture (store payload &optional provenance)
  "Capture exact local input independently of canonical unii-memory."
  (let* ((data (unii-memory-context-copy (list :payload payload :provenance provenance)))
         (id (format "%s:%s" (unii-memory-context-store-session store) (cl-incf (unii-memory-context-store-sequence store))))
         (entry (append (list :id id :terminal nil :outcome nil :events nil
                              :bytes (string-bytes (prin1-to-string data))) data)))
    (push entry (unii-memory-context-store-entries store))
    (unii-memory-context-prune store)
    id))
(defun unii-memory-context-finish (store id outcome)
  (let ((entry (cl-find id (unii-memory-context-store-entries store)
                        :test (function equal) :key (lambda (e) (plist-get e :id)))))
    (when (and entry (not (plist-get entry :terminal)))
      (setf (plist-get entry :terminal) t
            (plist-get entry :outcome) outcome)
      (unii-memory-context-prune store))))
(defun unii-memory-context-get (store id)
  "Return an isolated snapshot or explicit absence without guessing its cause."
  (let ((entry (cl-find id (unii-memory-context-store-entries store)
                        :test (function equal) :key (lambda (e) (plist-get e :id)))))
    (if entry (append (list :available t) (unii-memory-context-copy entry)) '(:available nil))))
(defun unii-memory-context-event (store id event)
  "Append isolated evidence without changing the captured input or outcome."
  (let ((entry (cl-find id (unii-memory-context-store-entries store)
                        :test #'equal :key (lambda (e) (plist-get e :id)))))
    (when entry
      (let ((copy (unii-memory-context-copy event)))
        (setf (plist-get entry :events)
              (append (plist-get entry :events) (list copy))
              (plist-get entry :bytes)
              (+ (plist-get entry :bytes) (string-bytes (prin1-to-string copy))))
        (unii-memory-context-prune store)))))
(provide 'unii-memory-context)
