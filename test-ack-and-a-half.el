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

(ert-deftest test-ack-and-a-half ()
  "Test that `ack-and-a-half' finds the correct results for literal search."
  (let ((data-dir (expand-file-name "samples/data"))
        (expected-file (expand-file-name "samples/expected.txt")))

    (ack-and-a-half "yourself" :directory data-dir)
    (while (get-buffer-process (get-buffer "*Ack-and-a-half*"))
      (sit-for 0.1))

    (let ((actual-content (test-ack-and-a-half--get-buffer-content))
          (expected-content (with-temp-buffer
                              (insert-file-contents expected-file)
                              (buffer-substring-no-properties (point-min) (point-max)))))
      (ert-info ((format "Actual content:\n%s\nExpected content:\n%s"
                         actual-content expected-content)
                 :prefix "Result: ")
                (should (string= actual-content expected-content))))))

(provide 'test-ack-and-a-halt)

;;; test-ack-and-a-half.el ends here
