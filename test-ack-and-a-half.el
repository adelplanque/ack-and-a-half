;; test-ack-and-a-half.el --- Tests for ack-and-a-half.el -*- lexical-binding: t; -*-

;;; Commentary:

;; This file contains automated tests for the `ack-and-a-half.el` package.
;; It uses ERT (Emacs Lisp Regression Testing).

;;; Code:

(require 'ert)
(when (require 'undercover nil t)
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
  "Create a new test case named NAME.

The test invoke `ack-and-a-half' to search PATTERN.
It then checks that the search result matches the contents of the EXPECTED file.

Keyword OPTIONS:
  :args      A plist of arguments passed to `ack-and-a-half'.
  :bindings  A list of variable bindings for the test context.
  :doc       A docstring describing the test."
  (let ((args      (plist-get options :args))
        (bindings  (plist-get options :bindings))
        (docstring (or (plist-get options :doc) (symbol-name name))))
    `(ert-deftest ,name () ,docstring
       (let ((args ,args)
             (expected-file (expand-file-name ,expected)))
         (unless (plist-member args :directory)
           (setq args (plist-put args :directory (expand-file-name "samples/data"))))
         (let ,bindings
           (apply #'ack-and-a-half ,pattern args)
           (while (get-buffer-process (get-buffer "*Ack-and-a-half*"))
             (sit-for 0.1))
           (let ((actual-content (test-ack-and-a-half--get-buffer-content))
                 (expected-content (with-temp-buffer
                                     (insert-file-contents ,expected)
                                     (buffer-substring-no-properties (point-min) (point-max)))))
             (ert-info ((format "Actual content:\n%s\nExpected content:\n%s"
                                actual-content expected-content)
                        :prefix "Result: ")
                       (should (string= actual-content expected-content)))))))))

(test-ack-and-a-half--define-test
 test1 "yourself" "samples/expected/test1.txt"
 :doc "Simple search with default configuration")

(test-ack-and-a-half--define-test
 regexp1 "yours?elf" "samples/expected/regexp1.txt"
 :doc "Explicit regexp search"
 :args (list :regexp t))

(test-ack-and-a-half--define-test
 regexp2 "yours?elf" "samples/expected/regexp2.txt"
 :doc "Explicit litteral search"
 :args (list :regexp nil))

(test-ack-and-a-half--define-test
 same1 "Lucrece" "samples/expected/same1.txt"
 :doc "Explicit all files search"
 :args (list :same nil))

(test-ack-and-a-half--define-test
 same2 "Lucrece" "samples/expected/same2.txt"
 :doc "Explicit same files search"
 :args (list :same t)
 :bindings ((major-mode 'python-mode)))

(test-ack-and-a-half--define-test
 ignore-dirs1 "Lucrece" "samples/expected/ignore-dirs1.txt"
 :doc "Explicit same files search"
 :args (list :same nil
             :ignore-dirs '("Shakespeare")))

(test-ack-and-a-half--define-test
 ignore-dirs2 "Lucrece" "samples/expected/ignore-dirs2.txt"
 :doc "Ignore dir using user customize"
 :args '(:same nil)
 :bindings ((ack-and-a-half-ignore-dirs '("Shakespeare"))))

(provide 'test-ack-and-a-halt)

;;; test-ack-and-a-half.el ends here
