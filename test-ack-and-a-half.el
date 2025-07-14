;; test-ack-and-a-half.el --- Tests for ack-and-a-half.el -*- lexical-binding: t; -*-

;;; Commentary:

;; This file contains automated tests for the `ack-and-a-half.el` package.
;; It uses ERT (Emacs Lisp Regression Testing).

;;; Code:

(require 'ert)
(when (and (>= emacs-major-version 26)
           (require 'undercover nil t))
  (setq undercover-force-coverage t)
  (undercover "ack-and-a-half.el"
              (:report-file "coverage.info")
              (:report-format 'lcov)
              (:merge-report nil)
              (:verbosity 10)
              (:send-report nil))
  (add-hook 'kill-emacs-hook
            (lambda () (let ((undercover--report-file-path "coverage.txt"))
                         (undercover-text--report)))))

(require 'ack-and-a-half)

(defun test-ack-and-a-half--get-buffer-content ()
  "Extract relevant content from the `*Ack-and-a-half*' buffer.

The buffer's start and end contain non-reproducible, context-sensitive text
such as timestamps or system paths.  This function isolates stable content
between the first and last error lines."
  (with-current-buffer (get-buffer "*Ack-and-a-half*")
    (if (condition-case nil
            (progn (compilation-next-error 1 nil (point-min)) t)
          (error) nil)
        (let* ((beg (point))
               (cur beg))
          (while (condition-case nil
                     (progn (compilation-next-error 1) t)
                   (error nil)))
          (forward-line)
          (buffer-substring-no-properties beg (point)))
      "")))

(defmacro test-ack-and-a-half--define-test (name pattern expected &rest options)
  "Define ERT test cases for `ack-and-a-half' using multiple backends.

Each test searches for PATTERN and compares results against EXPECTED, a file
path.

Tests are generated per backend and named NAME with suffix -backend.

Keyword OPTIONS:
  :args      A plist of arguments passed to `ack-and-a-half'.
  :bindings  A list of variable bindings for the test context.
  :doc       A docstring describing the test.
  :backends  A list of backend names to test (default (\"ack\" \"ripgrep\"))."
  (let ((args      (plist-get options :args))
        (bindings  (plist-get options :bindings))
        (docstring (or (plist-get options :doc) (symbol-name name)))
        (backends  (or (plist-get options :backends) '("ack" "ripgrep"))))
    `(progn
       ,@(mapcar
          (lambda (backend)
            `(ert-deftest
                 ,(intern (format "%s-%s" name backend)) () ,docstring
                 (let ((nargs ',(plist-put (copy-sequence args) :backend backend))
                       (expected-file (expand-file-name ,expected)))
                   (unless (plist-member nargs :directory)
                     (setq nargs (plist-put nargs :directory (expand-file-name "samples/data"))))
                   (let ,bindings
                     (apply #'ack-and-a-half ,pattern nargs)
                     (while (get-buffer-process (get-buffer "*Ack-and-a-half*"))
                       (sit-for 0.1))
                     (let ((actual-content (test-ack-and-a-half--get-buffer-content))
                           (expected-content
                            (with-temp-buffer
                              (insert-file-contents ,expected)
                              (buffer-substring-no-properties (point-min) (point-max)))))
                       (ert-info ((format "Actual content:\n%s\nExpected content:\n%s"
                                          actual-content expected-content)
                                  :prefix "Result: ")
                         (should (string= actual-content expected-content))))))))
          backends))))

(test-ack-and-a-half--define-test
 test1 "yourself" "samples/expected/test1.txt"
 :doc "Simple search with default configuration")

(test-ack-and-a-half--define-test
 regexp1 "yours?elf" "samples/expected/regexp1.txt"
 :doc "Explicit regexp search"
 :args (:regexp t))

(test-ack-and-a-half--define-test
 regexp2 "yours?elf" "samples/expected/regexp2.txt"
 :doc "Explicit litteral search"
 :args (:regexp nil))

(test-ack-and-a-half--define-test
 same1 "Lucrece" "samples/expected/same1.txt"
 :doc "Explicit all files search"
 :args (:same nil))

(test-ack-and-a-half--define-test
 same2 "Lucrece" "samples/expected/same2.txt"
 :doc "Explicit same files search"
 :args (:same t)
 :bindings ((major-mode 'python-mode)))

(test-ack-and-a-half--define-test
 ignore-dirs1 "Lucrece" "samples/expected/ignore-dirs1.txt"
 :doc "Explicit same files search"
 :args (:same nil :ignore-dirs ("Shakespeare")))

(test-ack-and-a-half--define-test
 ignore-dirs2 "Lucrece" "samples/expected/ignore-dirs2.txt"
 :doc "Ignore dir using user customize"
 :args (:same nil)
 :bindings ((ack-and-a-half-ignore-dirs '("Shakespeare"))))

(provide 'test-ack-and-a-halt)

;;; test-ack-and-a-half.el ends here
