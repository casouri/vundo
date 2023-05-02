;;; vundo-test.el --- Tests for vundo  -*- lexical-binding: t; -*-

;; Author: JD Smith

;;; This file is NOT part of GNU Emacs

;;; Commentary:

;; Stress testing the undo/vundo sytem.  Builds a (potentially large)
;; tree of edits in a custom buffer *vundo-stress-test*.  Requires the
;; lorem-ipsum package to be installed.  For testing large trees, you
;; may need to increase `undo-limit', `undo-strong-limit',
;; and `undo-outer-limit'.

;;; Code:

(require 'vundo)
(require 'lorem-ipsum)
(eval-when-compile
  (require 'cl-lib))
(require 'memory-report)

;; This is needed to prevent jitting errors during vundo navigation
(add-hook 'vundo-mode-hook (lambda () (setq jit-lock-mode nil)))

;;;###autoload
(defun vundo-stress-test (nedits nundos max-chain-length test-root-nav)
  "Stress-test vundo-mode and the undo system.
Interactively, prompts for the number of edits, undos, and
undo/redo chain depth.  NEDITS is the number of lorem-ipsum
sentences to insert which affects the total depth of the tree.
NUNDOS is the (approximate) number of undo/redo chains to
introduce into the edit sequence.  Larger values lead to wider
trees with more (sub-)branching.  MAX-CHAIN-LENGTH is the maximum
depth of the undo chains, which affects the length and nesting of
the branches.  Prefix argument TEST-ROOT-NAV enables benchmarking
navigation to the root of the undo tree.

Timing is reported for launching vundo, and (if TEST-ROOT-NAV is
non-nil) navigating to the tree's root.  Current undo limits,
buffer content size and undo-list memory usage are also reported.
Debug is enabled upon entry to vundo."
  (interactive
   (list (read-number "Number of edits (sentences): " 3000)
         (read-number "Approximate number of undo/redo chains: " 500)
         (read-number "Maximum undo length: " 14)
         (y-or-n-p "Perform root navigation benchmark? "))) ;*very* sensitive to this number
  (let ((obuf (get-buffer-create "*vundo-stress-test*"))
        (vundo-window-max-height 30))
    (with-current-buffer obuf
      (read-only-mode -1)
      (font-lock-mode -1)
      (setq buffer-undo-list nil
            pending-undo-list nil)
      (erase-buffer)
      (if-let ((vbuf (vundo-1 obuf)))
          (kill-buffer vbuf))
      (cl-loop for n upto nedits
               for r = (random nedits)
               do
               (lorem-ipsum-insert-sentences 1)
               (insert "\n")
               (undo-boundary)
               (when (<= r nundos)      ; insert an undo chain
                 (condition-case nil
                     (progn (undo (1+  (random max-chain-length)))
                            (undo-boundary))
                   (error (undo-boundary))))) ;may run out of undo length
      (display-buffer "*vundo-stress-test*")
      (message "> Undo stress test complete: %d sentences inserted, %d undo chains of max length %d"
               nedits nundos max-chain-length)
      (message "undo-limits: %d strong: %d outer: %d"
               undo-limit undo-strong-limit undo-outer-limit)
      (call-interactively #'count-words)
      (message "buffer-undo-list: length %7d, %9d bytes"
               (length buffer-undo-list)
               (memory-report-object-size buffer-undo-list))
      (message ">> Launching vundo:")
      (benchmark-progn (vundo))

      (when test-root-nav
        (message ">> Navigating to tree root:")
        (setq vundo--message nil
              cursor-type nil)
        (benchmark-progn (while (vundo-stem-root))))
      (vundo--debug)
      (let ((ul (buffer-local-value 'buffer-undo-list obuf)))
        (message "buffer-undo-list: length %7d, %9d bytes"
                 (length ul) (memory-report-object-size ul))))))

(provide 'vundo-stress-test)

;;; vundo-stress-test.el ends here
