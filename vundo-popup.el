;;; vundo-popup.el --- vundo as visualizer for the ordinary undo commands -*- lexical-binding: t -*-

;; Copyright (C) 2025 Free Software Foundation, Inc

;; Author: Michael Heerdegen <michael_heerdegen@web.de>
;; Maintainer: Michael Heerdegen <michael_heerdegen@web.de>
;; Created: 16 Feb 2025


;; This file is not part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; This file implements the minor mode `vundo-popup-mode'.  When
;; enabled, this mode makes using the ordinary undo commands display
;; vundo as a visual guide.  The popup is not selected and
;; automatically closed after a short timeout.
;;
;; TODO: Nothing yet?


;;; Code:

;;;; Requirements

(require 'vundo)


;;;; Configuration stuff

(defcustom vundo-popup-timeout 3.0
  "Time to keep vundo auto popups open, in seconds.
Only relevant for popups created automatically when using
`vundo-popup-mode'."
  :type 'number :group 'vundo)

(defcustom vundo-popup-window-min-height 1 ;IME a small height is nicer for a popup... 1 works well
  "Overwrites the default window-min-height of the vundo popup window.
nil means to use the default.  Only effects popups created by
`vundo-popup-mode'."
  :type '(choice
          (const  :tag "Use default" nil)
          (number :tag "Popup window min height"))
  :group 'vundo)

(defvar vundo-popup-commands `(,#'undo ,#'undo-only ,#'undo-redo)
  "List of commands that `vundo-popup-mode' modifies.")


;;;; Helpers and definitions

(defvar-local vundo-popup-window nil)
(defvar-local vundo-remove-popup-win-fun nil)

(defun vundo--trigger-function (dofun delay)
  "Return a function triggering calling DOFUN after DELAY."
  (let ((triggered nil) (timer nil))
    (letrec ((timer-fun
              (lambda ()
                (while-no-input
                  (when triggered
                    (funcall dofun)
                    (funcall abort-fun)))))
             (trigger-fun
              (lambda (&rest _)
                (setq triggered t)
                (when (timerp timer) (cancel-timer timer)) ;cleanest way to reset the timer AFAIK
                (setq timer (run-with-timer delay nil timer-fun))))
             (abort-fun
              (lambda ()
                (setq triggered nil)
                (when (timerp timer)
                  (cancel-timer timer)))))
      trigger-fun)))


;;;; Main Stuff

(defun vundo-prevent-popup-default-predicate ()
  "Prevent automatic vundo popup in the minibuffer."
  (minibufferp))

(defvar vundo-prevent-popup-predicate #'vundo-prevent-popup-default-predicate
  "Prevent vundo popup when funcalling the binding returns non-nil.")

(defun vundo-trigger-delete-popup-win-fun (buffer)
  "Return a closure triggering the deletion of BUFFER's vundo popup."
  ;; The whole popup mechanism works buffer locally.  Since the user may
  ;; switch buffers at any time and timers are global, the timer functions
  ;; that will close the popups must be closures.  The return value of this
  ;; function will start the timer when funcalled, but only once - when
  ;; called again the timeout for the current buffer is reset.
  (vundo--trigger-function
   (lambda ()
     (when (buffer-live-p buffer)
       (with-current-buffer buffer
         (when (and (windowp vundo-popup-window)
                    (window-live-p vundo-popup-window)
                    (not (eq vundo-popup-window (selected-window))))
           (delete-window vundo-popup-window))
         (setq-local vundo-popup-window nil))))
   vundo-popup-timeout))

(defun vundo--popup-advice (&rest _args)
  "Popup vundo window after executing the advised command."
  (unless (funcall vundo-prevent-popup-predicate)
    (let ((buffer-read-only buffer-read-only) ;this would be set by `vundo'
          (cb (current-buffer)))
      (save-selected-window
        (vundo)
        (with-current-buffer cb
          (setq-local vundo-popup-window (selected-window))
          (funcall
           (or vundo-remove-popup-win-fun
               (setq-local vundo-remove-popup-win-fun
                           (vundo-trigger-delete-popup-win-fun cb)))))))
    (let ((window-min-height ;vundo has a hardcoded 3, IMO too much for an auto popup
           (or vundo-popup-window-min-height
               window-min-height)))
      (fit-window-to-buffer vundo-popup-window vundo-window-max-height))))

;;;###autoload
(define-minor-mode vundo-popup-mode
  "Display a vundo popup when using any ordinary undo command."
  :group 'vundo
  (dolist (cmd vundo-popup-commands)
    (if vundo-popup-mode
        (advice-add cmd :after ;call CMD first: this way undoing in region works out of the box
                    #'vundo--popup-advice)
      (advice-remove cmd #'vundo--popup-advice))))


(provide 'vundo-popup)
;;; vundo-popup.el ends here
