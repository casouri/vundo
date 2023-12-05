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

(defun vundo-diff--cleanup-diff-buffer (orig buf current from to)
  "Update diff headers in BUF.
Headers are updated to indicate the diff in the contents of
buffer ORIG, between nodes FROM and TO, and given the CURRENT
node."
  (let* ((info (cl-loop for x in (list from to)
		        with oname = (buffer-name orig)
                        for ts = (vundo--mod-timestamp
			          vundo--prev-mod-list (vundo-m-idx x))
		        for idx = (vundo-m-idx x)
                        for stat = (if (eq x current) "Current"
				     (if vundo-diff--marked-node
				         "Marked" "Parent"))
		        collect
		        (list (format "<%s>[%s]" oname idx)
                              (format "<%s>[%d:%s]" oname idx stat)
		              (when (consp ts) (format-time-string "%F %r" ts)))))
	 (mxlen (apply #'max (mapcar (lambda (x) (length (cadr x))) info))))
    (dolist (x info) (setf (nth 1 x) (string-pad (nth 1 x) mxlen)))
    (with-current-buffer buf
      (goto-char (point-min))
      (let* ((inhibit-read-only t)
	     (change-files
	      (cl-loop for (name fullname ts) in info
		       for pat in '("---" "+++")
		       if (re-search-forward
			   (rx-to-string `(and bol ,pat (+ space)
					       (group (group (+ (not ?\t)))
						      (* any))
					       eol))
                           nil t)
		       collect (cons (match-string-no-properties 2) name)
		       and do (replace-match
                               (if ts (concat fullname "\t" ts) fullname)
			       t t nil 1)))
	     (lim (point)))
	(if (/= (length change-files) 2)
	    (message "NO LUCK %S\t%S" change-files info)
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
        (vundo--check-for-command
	 (vundo--move-to-node current marked orig vundo--prev-mod-list)
         (with-current-buffer mrkbuf
           (insert-buffer-substring-no-properties orig))
         (vundo--refresh-buffer orig (current-buffer) 'incremental)
         (vundo--move-to-node marked current orig vundo--prev-mod-list)
         (vundo--trim-undo-list orig current vundo--prev-mod-list)
         (vundo--refresh-buffer orig (current-buffer) 'incremental))
        (setq dbuf (diff-no-select (if swapped orig mrkbuf)
                                   (if swapped mrkbuf orig)
                                   nil nil (get-buffer-create
                                            (concat "*vundo-diff-" oname "*"))))
        
        (let* ((a (if swapped current marked))
               (b (if swapped marked current)))
          (if-let* ((proc (get-buffer-process dbuf)) ; diff called asynchronously
		    (orig-sentinel (process-sentinel proc)))
	      (set-process-sentinel
	       proc (lambda (&rest args)
		      (apply orig-sentinel args) 
		      (vundo-diff--cleanup-diff-buffer orig dbuf current a b)))
	    (vundo-diff--cleanup-diff-buffer orig dbuf current a b)))
        (kill-buffer mrkbuf)
        (display-buffer dbuf)))))

(provide 'vundo-diff)

;;; vundo-diff.el ends here
