;;; vundo-diff.el --- buffer diff for vundo      -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Free Software Foundation, Inc.
;;
;; Author: JD Smith <jdtsmith@gmail.com>
;; Maintainer: Yuan Fu <casouri@gmail.com>
;; URL: https://github.com/casouri/vundo
;; Version: 0.1
;; Package-Requires: ((emacs "28.1"))
;;
;; This file is part of GNU Emacs.
;;
;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; vundo-diff provides simple on-demand diff between arbitray undo
;; states in the vundo tree.  The additional key commands made
;; available are:
;;
;;    m  mark a node for diff
;;    u  unmark the marked node
;;    d  diff between the marked (or parent) and current nodes
;;    
;; All of the normal navigation command of vundo work as normal.

;;; Code:
(require 'vundo)
(require 'diff)

(defface vundo-diff-highlight
  '((((background light)) .
     (:inherit vundo-node :foreground "DodgerBlue4"))
    (((background dark)) .
     (:inherit vundo-node  :foreground "DodgerBlue1")))
  "Face for nodes marked for diff in the undo tree.")

(defvar-local vundo-diff--marked-node nil)
(defvar-local vundo-diff--highlight-overlay nil
  "Overlay used to highlight the selected node.")

;;;###autoload
(defun vundo-diff-mark (&optional node)
  "Mark NODE for vundo diff.
NODE defaults to the current node."
  (interactive)
  (let* ((mod-list vundo--prev-mod-list)
         (node (or node (vundo--current-node mod-list))))
    (setq vundo-diff--marked-node node)
    (unless vundo-diff--highlight-overlay
      (setq vundo-diff--highlight-overlay
            (make-overlay (1- (vundo-m-point node)) (vundo-m-point node)))
      (overlay-put vundo-diff--highlight-overlay
                   'display (vundo--translate "●"))
      (overlay-put vundo-diff--highlight-overlay
                   'face 'vundo-diff-highlight)
      (overlay-put vundo-diff--highlight-overlay 'priority 2))
    (move-overlay vundo-diff--highlight-overlay
                  (1- (vundo-m-point node))
                  (vundo-m-point node))))

;;;###autoload
(defun vundo-diff-unmark ()
  "Unmark the node marked for vundo diff."
  (interactive)
  (when vundo-diff--marked-node
    (setq vundo-diff--marked-node nil)
    (when vundo-diff--highlight-overlay
      (delete-overlay vundo-diff--highlight-overlay)
      (setq vundo-diff--highlight-overlay nil))))

;;;###autoload
(defun vundo-diff ()
  "Perform diff between marked and current buffer state.
Displays in a separate diff buffer with name based on
the original buffer name."
  (interactive)
  (let* ((orig vundo--orig-buffer)
         (oname (buffer-name orig))
         (current (vundo--current-node vundo--prev-mod-list))
         (marked (or vundo-diff--marked-node (vundo-m-parent current)))
         (swapped (> (vundo-m-idx marked) (vundo-m-idx current)))
         dbuf)
    (if (or (not current) (not marked) (eq current marked))
        (message "vundo diff not available.")
      (let ((mrkbuf (get-buffer-create
                     (make-temp-name (concat oname "-vundo-diff-marked")))))
        (vundo--move-to-node current marked orig vundo--prev-mod-list)
        (with-current-buffer mrkbuf
          (insert-buffer-substring-no-properties orig))
        (vundo--refresh-buffer orig (current-buffer) 'incremental)
        (vundo--move-to-node marked current orig vundo--prev-mod-list)
        (vundo--refresh-buffer orig (current-buffer) 'incremental)
        (setq dbuf (diff-no-select (if swapped orig mrkbuf)
                                   (if swapped mrkbuf orig)
                                   nil nil (get-buffer-create
                                            (concat "*vundo-diff-" oname "*"))))
        (let* ((inhibit-read-only t)
               (a (if swapped current marked))
               (b (if swapped marked current))
               (diff-msg
                (concat
                 (propertize " vundo-diff: " 'font-lock-face 'diff-header)
                 (propertize oname 'font-lock-face '(diff-file-header diff-header))
                 "\n"
                 (apply #'format "  %4s: %7s[%s]%s\n  %4s: %7s[%s]%s\n\n"
                        (cl-loop
                         for x in (list a b) for h in '("From" "To")
                         collect
                         (propertize h 'font-lock-face 'diff-header)
                         collect
                         (propertize (if (eq x current) "Current"
                                       (if vundo-diff--marked-node
                                           "Marked" "Parent"))
                                     'font-lock-face
                                     (if (or (eq x current)
                                             (null vundo-diff--marked-node))
                                         'vundo-highlight 'vundo-diff-highlight))
                         collect
                         (propertize (number-to-string (vundo-m-idx x))
                                     'font-lock-face
                                     '(diff-file-header diff-header))
                         collect
                         (if-let* ((vundo-highlight-saved-nodes)
                                   (ts (vundo--mod-timestamp
                                        vundo--prev-mod-list (vundo-m-idx x)))
                                   ((consp ts)))
                             (propertize
                              (format " Saved: %s" (format-time-string "%F %r" ts))
                              'font-lock-face 'diff-header)
                           ""))))))
          (with-current-buffer dbuf
            (goto-char (point-min))
            (insert diff-msg)))
        (kill-buffer mrkbuf)
        (display-buffer dbuf)))))

(provide 'vundo-diff)

;;; vundo-diff.el ends here
