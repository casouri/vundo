;;; vundo-test.el --- Tests for vundo  -*- lexical-binding: t; -*-

;; Author: Yuan Fu <casouri@gmail.com>

;;; This file is NOT part of GNU Emacs

;;; Commentary:

;;; Code:

(require 'ert)
(require 'vundo)
(require 'subr-x)
(require 'cl-lib)

(ert-deftest vundo-test--mod-list ()
  "Tests for mod-list generation and incremental extension."
  (let* ((ul3 '(m3 m2 m1))
         (ul6 (append '(m6 m5 m4 nil) ul3))
         (ul9 (append '(m9 m8 m7 nil) ul6))
         (ml6 (vundo--mod-list-from ul6 7 nil))
         (ml9 (vundo--mod-list-from ul9 4 ml6)))
    (should (equal (mapcar #'vundo-m-undo-list ml6)
                   (list nil ul3 ul6)))
    (should (equal (mapcar #'vundo-m-undo-list ml9)
                   (list nil ul3 ul6 ul9)))

    (let ((ht6 (vundo--update-mapping ml6 nil 0)))
      (should (eq (gethash ul3 ht6) (aref ml6 1)))
      (should (eq (gethash ul6 ht6) (aref ml6 2)))

      (let ((ht9 (vundo--update-mapping ml9 ht6 3)))
        (should (eq (gethash ul9 ht9) (aref ml9 3))))

      (should (equal (mapcar #'vundo-m-idx ml9)
                     '(0 1 2 3))))))

(ert-deftest vundo-test--position-only-p ()
  "Tests for ‘vundo--position-only-p’."
  (let* ((ul1 '(9 nil 8 nil 7 nil))
         (ul2 '(9 nil 8 stuff nil 7 nil)))
    (should (vundo--position-only-p ul1))
    (should (vundo--position-only-p (nthcdr 2 ul1)))
    (should (vundo--position-only-p (nthcdr 4 ul1)))
    (should (vundo--position-only-p (last ul1)))

    (should (vundo--position-only-p ul2))
    (should (not (vundo--position-only-p (nthcdr 2 ul2))))
    (should (vundo--position-only-p (nthcdr 5 ul2)))))

(ert-deftest vundo-test--skip-position-only ()
  "Tests for skipping position-only records."
  (let* ((ul2 '(stuff nil 9 nil 8 stuff nil 7 nil)))
    (should (equal (length (vundo--mod-list-from ul2))
                   3))))

(defsubst vundo-test--buf-str-np ()
  "(buffer-substring-no-properties (point-min) (point-max))."
  (buffer-substring-no-properties
   (point-min) (point-max)))

(defsubst vundo-test--current-idx ()
  "(vundo-m-idx (vundo--current-node vundo--prev-mod-list))."
  (vundo-m-idx (vundo--current-node vundo--prev-mod-list)))

(defsubst vundo-test--last-idx ()
  "(vundo-m-idx (car (last vundo--prev-mod-list)))."
  (vundo-m-idx (aref vundo--prev-mod-list (1- (length vundo--prev-mod-list)))))

(defmacro vundo-test--setup (&rest body)
  "Setup and evaluate BODY."
  `(with-temp-buffer
     (buffer-enable-undo)
     (let ((vundo-glyph-alist vundo-unicode-symbols))
       ,@body)))

(ert-deftest vundo-test--1 ()
  "Tests for simple movements.
Tests backward/forward, previous/next, undo list trimming."
  (vundo-test--setup
   (insert "a")
   (undo-boundary)
   (undo)
   (undo-boundary)
   (insert "a")
   (undo-boundary)

   (with-current-buffer (vundo-1 (current-buffer))
     (should (equal (vundo-test--buf-str-np)
                    "○──○\n└──○"))
     (should (eq (vundo-test--current-idx) 3))

     (vundo-next 1)
     (should (eq (vundo-test--current-idx) 1))

     (vundo-backward 1)
     (should (eq (vundo-test--current-idx) 0))

     (vundo-forward 1)
     (should (eq (vundo-test--current-idx) 3))

     (dotimes (_ 20)
       (vundo-backward 1)
       (vundo-forward 1))
     (should (eq (length vundo--prev-mod-list) 4))

     (dotimes (_ 20)
       (vundo-next 1)
       (vundo-previous 1))
     (should (eq (length vundo--prev-mod-list) 4)))))

(defun vundo--file-content (file)
  "Return the content of FILE as a string."
  (with-temp-buffer
    (insert-file-contents file)
    (vundo-test--buf-str-np)))

(defvar vundo--test-directory
  (or (and load-file-name
           (file-name-directory load-file-name))
      default-directory)
  "Path to test directory under project root.")

(defun vundo--test-file (file)
  "Return the content of FILE under test directory.
Sans ending newline."
  (string-trim (vundo--file-content
                (expand-file-name file vundo--test-directory))
               "\n"))

(defun vundo-test--insert (&rest strings)
  "Insert STRINGS, delimit them with (undo-boundary)."
  (let ((inhibit-read-only t))
    (dolist (str strings)
      (insert str)
      (undo-boundary))))

(defun vundo-test--exec (cmds)
  "Run commands in CMDS like interactive commands."
  (dolist (cmd cmds)
    (setq last-command this-command)
    (setq this-command cmd)
    (run-hooks 'pre-command-hook)
    (command-execute cmd)
    (run-hooks 'post-command-hook)
    (undo-boundary)))

(ert-deftest vundo-test--2 ()
  "This tests high trees (100 nodes tall)."
  (vundo-test--setup
   (dotimes (_ 100)
     (vundo-test--insert "a"))
   (with-current-buffer (vundo-1 (current-buffer))
     (vundo-backward 50))
   (dotimes (_ 50)
     (vundo-test--insert "a"))

   (with-current-buffer (vundo-1 (current-buffer))
     (should (equal (vundo-test--buf-str-np)
                    (vundo--test-file "test-2.txt")))
     (should (eq (vundo-test--last-idx) 200))

     (vundo-backward 50)
     (should (eq (vundo-test--last-idx) 250))

     (vundo-forward 1)
     (vundo-next 1)
     (should (eq (vundo--get-node-at-point)
                 (aref vundo--prev-mod-list 51)))

     (dotimes (_ 20)
       (vundo-previous 1)
       (vundo-next 1))
     (should (eq (vundo--get-node-at-point)
                 (aref vundo--prev-mod-list 51)))

     (dotimes (_ 20)
       (vundo-forward 49)
       (vundo-backward 49))
     (should (eq (vundo--get-node-at-point)
                 (aref vundo--prev-mod-list 51))))))

(ert-deftest vundo-test--3 ()
  "This tests regional undos."
  ;; Emacs do weird things with region and mark in batch mode. Not
  ;; sure the reason, but in batch mode the first undo we perform is
  ;; not regional, and the test will fail.
  (ert--skip-unless (not noninteractive))
  (vundo-test--setup
   (vundo-test--insert "a" "b" "c" "d" "e")
   ;; Undo in region that covers "ab".
   (vundo-test--exec '(move-beginning-of-line
                       push-mark-command
                       forward-char
                       forward-char
                       undo))
   (should (equal (vundo-test--buf-str-np) "acde"))
   ;; The regional undo should create a new node.
   (with-current-buffer (vundo-1 (current-buffer))
     (should (equal (vundo-test--buf-str-np)
                    "○──○──○──○──○──○──○"))
     (vundo-backward 1))
   (should (equal (vundo-test--buf-str-np) "abcde"))

   (vundo-test--exec '(move-end-of-line))
   (vundo-test--insert "f" "g" "h")
   (with-current-buffer (vundo-1 (current-buffer))
     (should (eq (vundo-test--current-idx) 10)))))

(defun vundo-test--count (string buffer)
  "Return the count of STRING in BUFFER."
  (with-current-buffer buffer
    (goto-char (point-min))
    (let ((count 0))
      (while (search-forward string nil t)
        (cl-incf count))
      count)))

(ert-deftest vundo-test--4 ()
  "This tests large buffers."
  (vundo-test--setup
   (let ((undo-limit most-positive-fixnum)
         (undo-strong-limit most-positive-fixnum)
         (inhibit-read-only t))
     (dotimes (_1 100)
       (dotimes (_2 5000)
         ;; Not DRY, but I like to write it.
         (insert "TANK!THE!BEST!"))
       (insert "\n")
       (undo-boundary))
     (should (eq (vundo-test--count
                  "TANK!THE!BEST!" (current-buffer))
                 500000))
     (with-current-buffer (vundo-1 (current-buffer))
       (should (eq (length vundo--prev-mod-list) 101))
       (dotimes (_ 50)
         (vundo-backward 1)))
     ;; Test if the buffer content is as expected.
     (should (eq (vundo-test--count
                  "TANK!THE!BEST!" (current-buffer))
                 250000))
     ;; Create a new branch
     (dotimes (_1 50)
       (dotimes (_2 5000)
         (insert "WO QUI NON COIN"))
       (insert "\n")
       (undo-boundary))
     ;; Go back to the tip of the original branch.
     (with-current-buffer (vundo-1 (current-buffer))
       (should (eq (length vundo--prev-mod-list) 201))
       (dotimes (_ 49)
         (vundo-backward 1))
       (vundo-next 1)
       (dotimes (_ 50)
         (vundo-forward 1)))
     ;; And check if the buffer content is as expected.
     (should (eq (vundo-test--count
                  "TANK!THE!BEST!" (current-buffer))
                 500000)))
   (garbage-collect)))

(provide 'vundo-test)

;;; vundo-test.el ends here
