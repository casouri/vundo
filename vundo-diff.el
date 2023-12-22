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
;; states in the vundo tree.

;;; Code:
(require 'vundo)
(require 'diff)
(require 'diff-mode)
(eval-when-compile (require 'cl-lib))

(defface vundo-diff-highlight
  '((((background light)) .
     (:inherit vundo-highlight :foreground "DodgerBlue4"))
    (((background dark)) .
     (:inherit vundo-highlight  :foreground "DodgerBlue1")))
  "Face for nodes marked for diff in the undo tree.")

(defvar-local vundo-diff--marked-node nil)
(defvar-local vundo-diff--highlight-overlay nil
  "Overlay used to highlight the selected node.")

(defun vundo-diff--cleanup-diff-buffer (orig-name buf current from to)
  "Update diff headers in BUF.
Headers are updated to indicate the diff in the contents of
buffer named ORIG-NAME, between nodes FROM and TO, and given the
CURRENT node."
  (let ((inhibit-read-only t)
        (info (cl-loop for x in (list from to)
                       for idx = (vundo-m-idx x)
                       for ts = (vundo--node-timestamp vundo--prev-mod-list x)
                       for stat = (if (eq x current) "Current"
                                    (if vundo-diff--marked-node "Marked" "Parent"))
                       collect
                       (list (format "[%d]" idx)
                             (format "<%s> [mod %d] (%s)" orig-name idx stat)
                             (when (consp ts) (format-time-string "%F %r" ts)))))
        lim)
    (with-current-buffer buf
      (vundo-diff-mode)
      (goto-char (point-min))
      (insert (concat (propertize "vundo-diff: " 'font-lock-face 'diff-header)
                      (propertize  orig-name 'font-lock-face
                                   '(diff-file-header diff-header))
                      "\n"))
      (let* ((change-files
              (cl-loop for (name fullname ts) in info
                       for pat in '("---" "+++")
                       if (re-search-forward
                           (rx-to-string `(and bol ,pat (+ blank)
                                               (group (group (+ (not (any ?\n ?\t))))
                                                      (* nonl))
                                               eol))
                           nil t)
                       collect (cons (match-string-no-properties 2) name)
                       and do
                       (unless lim (setq lim (match-beginning 0)))
                       (replace-match
                        (if ts (concat fullname "\t" ts) fullname)
                        t t nil 1))))
        (when (eq (length change-files) 2)
          (goto-char (point-min))
          (dolist (c change-files) ; change the file names in the diff
            (when (search-forward (car c) lim t)
              (replace-match (cdr c)))))))))

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
      (overlay-put vundo-diff--highlight-overlay 'priority 1))
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
         swapped
         mrkbuf)
    (if (or (not current) (not marked) (eq current marked))
        (message "vundo diff not available.")
      (setq swapped (> (vundo-m-idx marked) (vundo-m-idx current)))
      (setq mrkbuf (get-buffer-create
                    (make-temp-name (concat oname "-vundo-diff-marked"))))
      (unwind-protect
          (progn
            (vundo--check-for-command
             (vundo--move-to-node current marked orig vundo--prev-mod-list)
             (with-current-buffer mrkbuf
               (insert-buffer-substring-no-properties orig))
             (vundo--refresh-buffer orig (current-buffer) 'incremental)
             (vundo--move-to-node marked current orig vundo--prev-mod-list)
             (vundo--trim-undo-list orig current vundo--prev-mod-list)
             (vundo--refresh-buffer orig (current-buffer) 'incremental))
            (let* ((diff-use-labels nil) ; We let our cleanup handle this.
                   (a (if swapped current marked))
                   (b (if swapped marked current))
                   (abuf (if swapped orig mrkbuf))
                   (bbuf (if swapped mrkbuf orig))
                   (dbuf (diff-no-select
                          abuf bbuf nil t
                          (get-buffer-create
                           (concat "*vundo-diff-" oname "*")))))
              (vundo-diff--cleanup-diff-buffer oname dbuf current a b)
              (display-buffer dbuf)))
        (kill-buffer mrkbuf)))))

(defconst vundo-diff-font-lock-keywords
  `((,(rx bol (or "---" "+++") (* nonl) "[mod " (group (+ num)) ?\]
          (+ ?\s) ?\((group (or "Parent" "Current")) ?\))
     (1 'diff-index t)
     (2 'vundo-highlight t))
    (,(rx bol (or "---" "+++") (* nonl) "[mod " (group (+ num)) ?\]
          (+ ?\s) ?\((group "Marked") ?\))
     (1 'diff-index t)
     (2 'vundo-diff-highlight t)))
  "Additional font-lock keyword to fontify Parent/Current/Marked.")

(define-derived-mode vundo-diff-mode diff-mode "Vundo Diff"
  :syntax-table nil
  :abbrev-table nil
  (setcar font-lock-defaults
          (append diff-font-lock-keywords vundo-diff-font-lock-keywords)))

(provide 'vundo-diff)

;;; vundo-diff.el ends here

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
