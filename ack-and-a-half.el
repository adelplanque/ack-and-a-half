;;; ack-and-a-half.el --- Yet another front-end for ack -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2013 Jacob Helwig <jacob@technosorcery.net>
;; Alexey Lebedeff <binarin@binarin.ru>
;; Andrew Pennebaker <andrew.pennebaker@gmail.com>
;; Andrew Stine <stine.drew@gmail.com>
;; Derek Chen-Becker <derek@precog.com>
;; Gleb Peregud <gleber.p@gmail.com>
;; Kim van Wyk <vanwykk@gmail.com>
;; Lars Andersen <expez@expez.com>
;; Ronaldo M. Ferraz <ronaldoferraz@gmail.com>
;; Ryan Thompson <rct@thompsonclan.org>
;;
;; Author: Jacob Helwig <jacob+ack@technosorcery.net>
;; Homepage: http://technosorcery.net
;; Version: 1.2.0
;; URL: https://github.com/jhelwig/ack-and-a-half
;;
;; This file is NOT part of GNU Emacs.
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy of
;; this software and associated documentation files (the "Software"), to deal in
;; the Software without restriction, including without limitation the rights to
;; use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is furnished to do
;; so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.
;;
;;; Commentary:
;;
;; ack-and-a-half.el provides a simple compilation mode for the perl
;; grep-a-like ack (http://petdance.com/ack/).
;;
;; Add the following to your .emacs:
;;
;;     (add-to-list 'load-path "/path/to/ack-and-a-half")
;;     (require 'ack-and-a-half)
;;     (defalias 'ack 'ack-and-a-half)
;;     (defalias 'ack-same 'ack-and-a-half-same)
;;     (defalias 'ack-find-file 'ack-and-a-half-find-file)
;;     (defalias 'ack-find-file-same 'ack-and-a-half-find-file-same)
;;
;; Run `ack' to search for all files and `ack-same' to search for
;; files of the same type as the current buffer.
;;
;; `next-error' and `previous-error' can be used to jump to the
;; matches.
;;
;; `ack-find-file' and `ack-find-same-file' use ack to list the files
;; in the current project.  It's a convenient, though slow, way of
;; finding files.
;;

;;; Code:

(require 'compile)
(require 'grep)
(require 'thingatpt)
(require 'symbol-overlay nil t)

(add-to-list 'debug-ignored-errors
             "^Moved \\(back before fir\\|past la\\)st match$")
(add-to-list 'debug-ignored-errors "^File .* not found$")

(define-compilation-mode ack-and-a-half-mode "Ack"
  "Ack results compilation mode."
  (set (make-local-variable 'truncate-lines) t)
  (set (make-local-variable 'compilation-disable-input) t)
  (let ((smbl  'compilation-ack-nogroup)
        (pttrn '("^\\([^:\n]+?\\):\\([0-9]+\\):\\([0-9]+\\):" 1 2 3)))
    (set (make-local-variable 'compilation-error-regexp-alist) (list smbl))
    (set (make-local-variable 'compilation-error-regexp-alist-alist) (list (cons smbl pttrn))))
  (set (make-local-variable 'compilation-process-setup-function) 'ack-and-a-half-mode-setup)
  (set (make-local-variable 'compilation-error-face) grep-hit-face))

(defgroup ack-and-a-half nil "Yet another front end for ack."
  :group 'tools
  :group 'matching)

(defcustom ack-and-a-half-executable (or (executable-find "ack")
                                         (executable-find "ack-grep"))
  "*The location of the ack executable."
  :group 'ack-and-a-half
  :type 'file)

(defcustom ack-and-a-half-buffer-name "*Ack-and-a-half*"
  "*The name of the `ack-and-a-half' buffer."
  :group 'ack-and-a-half
  :type 'string)

(defcustom ack-and-a-half-arguments nil
  "*Extra arguments to pass to ack."
  :group 'ack-and-a-half
  :type '(repeat (string)))

(defcustom ack-and-a-half-mode-type-alist nil
  "*File type(s) to search per major mode.  (ack-and-a-half-same)
This overrides values in `ack-and-a-half-mode-type-default-alist'.
The car in each list element is a major mode, and the rest
is a list of strings passed to the --type flag of ack when running
`ack-and-a-half-same'."
  :group 'ack-and-a-half
  :type '(repeat (cons (symbol :tag "Major mode")
                       (repeat (string :tag "ack --type")))))

(defcustom ack-and-a-half-mode-extension-alist nil
  "*File extensions to search per major mode.  (ack-and-a-half-same)
This overrides values in `ack-and-a-half-mode-extension-default-alist'.
The car in each list element is a major mode, and the rest
is a list of file extensions to be searched in addition to
the type defined in `ack-and-a-half-mode-type-alist' when
running `ack-and-a-half-same'."
  :group 'ack-and-a-half
  :type '(repeat (cons (symbol :tag "Major mode")
                       (repeat :tag "File extensions" (string)))))

(defcustom ack-and-a-half-ignore-case 'smart
  "*Whether or not to ignore case when searching.
The special value \\='smart enables the ack option \"smart-case\"."
  :group 'ack-and-a-half
  :type '(choice (const :tag "Case sensitive" nil)
                 (const :tag "Smart case" smart)
                 (const :tag "Case insensitive" t)))

(defcustom ack-and-a-half-regexp-search t
  "*Default to regular expression searching.
Giving a prefix argument to `ack-and-a-half' toggles this option."
  :group 'ack-and-a-half
  :type '(choice (const :tag "Literal searching" nil)
                 (const :tag "Regular expression searching" t)))

(defcustom ack-and-a-half-use-environment t
  "*Use .ackrc and ACK_OPTIONS when searching."
  :group 'ack-and-a-half
  :type '(choice (const :tag "Ignore environment" nil)
                 (const :tag "Use environment" t)))

(defcustom ack-and-a-half-root-directory-functions '(ack-and-a-half-guess-project-root)
  "*List of functions used to find the base directory to ack from.
These functions are called until one returns a directory.  If
successful, `ack-and-a-half' is run from that directory instead
of from `default-directory'.  The directory is verified by the
user depending on `ack-and-a-half-prompt-for-directory'."
  :group 'ack-and-a-half
  :type '(repeat function))

(defcustom ack-and-a-half-project-root-file-patterns
  '(".project\\'"
    ".xcodeproj\\'"
    ".sln\\'"
    "\\`Project.ede\\'"
    "\\`.git\\'"
    "\\`.bzr\\'"
    "\\`_darcs\\'"
    "\\`.hg\\'")
  "*List of file patterns for the project root.
Each element is a regular expression.  If a file matching any
element is found in a directory, then that directory is assumed
to be the project root by `ack-and-a-half-guess-project-root'."
  :group 'ack-and-a-half
  :type '(repeat (string :tag "Regular expression")))

(defcustom ack-and-a-half-prompt-for-directory 'unless-guessed
  "*Prompt for directory in which to run ack.
If this is \\='unless-guessed, then the value determined by
`ack-and-a-half-root-directory-functions' is used without
confirmation.  If it is nil, then the directory is never
confirmed.  If t, then always prompt for the directory to use."
  :group 'ack-and-a-half
  :type '(choice (const :tag "Don't prompt" nil)
                 (const :tag "Don't prompt when guessed" unless-guessed)
                 (const :tag "Always prompt" t)))

(defcustom ack-and-a-half-use-ido nil
  "Whether or not `ack-and-a-half' should use ido.
Used to provide completion suggestions when prompting for directory."
  :type 'boolean)

(defcustom ack-and-a-half-ignore-dirs nil
  "List of directories to be ignored by ack command.

This will be append as `--ignore-dir' parameters of the `ack'
commande.  Variable is Buffer-Local and can be customized
differently depending on the mode."
  :group 'ack-and-a-half
  :type '(repeat string))
(make-variable-buffer-local 'ack-and-a-half-ignore-dirs)

;;; Default setting lists ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst ack-and-a-half-mode-type-default-alist
  '((actionscript-mode "actionscript")
    (LaTeX-mode "tex")
    (TeX-mode "tex")
    (asm-mode "asm")
    (batch-file-mode "batch")
    (c++-mode "cpp")
    (c-mode "cc")
    (cfmx-mode "cfmx")
    (cperl-mode "perl")
    (csharp-mode "csharp")
    (css-mode "css")
    (emacs-lisp-mode "elisp")
    (erlang-mode "erlang")
    (espresso-mode "java")
    (fortran-mode "fortran")
    (go-mode "go")
    (haskell-mode "haskell")
    (hexl-mode "binary")
    (html-mode "html")
    (java-mode "java")
    (javascript-mode "js")
    (jde-mode "java")
    (js2-mode "js")
    (jsp-mode "jsp")
    (latex-mode "tex")
    (lisp-mode "lisp")
    (lua-mode "lua")
    (makefile-gmake-mode "make")
    (makefile-mode "make")
    (markdown-mode "markdown")
    (mason-mode "mason")
    (nxml-mode "xml")
    (objc-mode "objc" "objcpp")
    (ocaml-mode "ocaml")
    (parrot-mode "parrot")
    (perl-mode "perl")
    (php-mode "php")
    (plone-mode "plone")
    (python-mode "python")
    (ruby-mode "ruby")
    (scala-mode "scala")
    (scheme-mode "scheme")
    (sh-mode "shell")
    (shell-script-mode "shell")
    (skipped-mode "skipped")
    (smalltalk-mode "smalltalk")
    (sql-mode "sql")
    (tcl-mode "tcl")
    (tex-mode "tex")
    (tt-mode "tt")
    (vb-mode "vb")
    (vim-mode "vim")
    (xml-mode "xml")
    (yaml-mode "yaml"))
  "Default values for `ack-and-a-half-mode-type-alist'.")

(defconst ack-and-a-half-mode-extension-default-alist
  '((d-mode "d"))
  "Default values for `ack-and-a-half-mode-extension-alist'.")

(defun ack-and-a-half-create-type (extensions)
  "Ack options to filter files by EXTENSIONS."
  (list "--type" "customtype" "--type-set"
        (concat "customtype:ext:" (mapconcat 'identity extensions ","))))

(defun ack-and-a-half-type-for-major-mode (mode)
  "Return the --type and --type-set arguments to use with ack for major mode MODE."
  (let ((types (cdr (or (assoc mode ack-and-a-half-mode-type-alist)
                        (assoc mode ack-and-a-half-mode-type-default-alist))))
        (ext (cdr (or (assoc mode ack-and-a-half-mode-extension-alist)
                      (assoc mode ack-and-a-half-mode-extension-default-alist))))
        result)
    (dolist (type types)
      (push type result)
      (push "--type" result))
    (if ext
        (if types
            `("--type-add" ,(concat (car types)
                                    "=" (mapconcat 'identity ext ","))
              . ,result)
          (ack-and-a-half-create-type ext))
      result)))

;;; Project root ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ack-and-a-half-guess-project-root ()
  "Guess the project root directory.
This is intended to be used in `ack-and-a-half-root-directory-functions'."
  (catch 'root
    (let ((dir (expand-file-name (if buffer-file-name
                                     (file-name-directory buffer-file-name)
                                   default-directory)))
          (pattern (mapconcat 'identity ack-and-a-half-project-root-file-patterns "\\|")))
      (while (not (equal dir "/"))
        (when (directory-files dir nil pattern t)
          (throw 'root dir))
        (setq dir (file-name-directory (directory-file-name dir)))))))

;;; Commands ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar ack-and-a-half-directory-history nil
  "Directories recently searched with `ack-and-a-half'.")
(defvar ack-and-a-half-literal-history nil
  "Strings recently searched for with `ack-and-a-half'.")
(defvar ack-and-a-half-regexp-history nil
  "Regular expressions recently searched for with `ack-and-a-half'.")
(defvar ack-and-a-half-extra-args-history nil
  "Extra args recents passed to ack with `ack-and-a-half-with-args'.")

(defun ack-and-a-half-initial-contents-for-read ()
  (when (ack-and-a-half-use-region-p)
    (buffer-substring-no-properties (region-beginning) (region-end))))

(defun ack-and-a-half-default-for-read ()
  "Determine the pattern to search based on the buffer context."
  (let ((pattern (unless (ack-and-a-half-use-region-p) (thing-at-point 'symbol))))
    (set-text-properties 0 (length pattern) nil pattern)
    pattern))

(defun ack-and-a-half-use-region-p ()
  (or (and (fboundp 'use-region-p) (use-region-p))
      (and transient-mark-mode mark-active
           (> (region-end) (region-beginning)))))

(defsubst ack-and-a-half-read (regexp)
  (let* ((default (ack-and-a-half-default-for-read))
         (type (if regexp "pattern" "literal search"))
         (prompt  (if default
                      (format "ack %s (default %s): " type default)
                    (format "ack %s: " type))))
    (read-string prompt
                 (ack-and-a-half-initial-contents-for-read)
                 (if regexp 'ack-and-a-half-regexp-history 'ack-and-a-half-literal-history)
                 default)))

(defun ack-and-a-half-read-dir ()
  "Ask user for a directory."
  (let ((dir (run-hook-with-args-until-success 'ack-and-a-half-root-directory-functions)))
    (if ack-and-a-half-prompt-for-directory
        (if (and dir (eq ack-and-a-half-prompt-for-directory 'unless-guessed))
            dir
          (if ack-and-a-half-use-ido
              (ido-read-directory-name "Directory: " dir dir t)
            (read-directory-name "Directory: " dir dir t)))
      (or dir
          (and buffer-file-name (file-name-directory buffer-file-name))
          default-directory))))

(defsubst ack-and-a-half-xor (a b)
  (if a (not b) b))

(defun ack-and-a-half-interactive ()
  "Return the (interactive) arguments.
Used by `ack-and-a-half' and `ack-and-a-half-same'."
  (let ((regexp (ack-and-a-half-xor current-prefix-arg ack-and-a-half-regexp-search)))
    (list (ack-and-a-half-read regexp)
          regexp
          (ack-and-a-half-read-dir))))

(defun ack-and-a-half-type ()
  "Return type argument to pass to ack command."
  (or (ack-and-a-half-type-for-major-mode major-mode)
      (when buffer-file-name
        (ack-and-a-half-create-type (list (file-name-extension buffer-file-name))))))

(defun ack-and-a-half-option (name enabled)
  (format "--%s%s" (if enabled "" "no") name))

(defun ack-and-a-half-arguments-from-options (regexp)
  "Build arguments for ack command.
When REGEXP is nil use literal search"
  (let ((arguments (list "--nocolor" "--nogroup" "--column"
                         (ack-and-a-half-option "smart-case" (eq ack-and-a-half-ignore-case 'smart))
                         (ack-and-a-half-option "env" ack-and-a-half-use-environment))))
    (when ack-and-a-half-ignore-dirs
      (dolist (item ack-and-a-half-ignore-dirs)
        (setq arguments (append arguments `("--ignore-dir" ,(shell-quote-argument item))))))
    (unless ack-and-a-half-ignore-case
      (push "-i" arguments))
    (unless regexp
      (push "--literal" arguments))
    arguments))

(defun ack-and-a-half-run (directory regexp pattern &rest arguments)
  "Run ack to search PATTERN in DIRECTORY with ARGUMENTS.
When REGEXP is nil, use literal search."
  (let* ((default-directory (if directory
                                (file-name-as-directory (expand-file-name directory))
                              default-directory))
         (cmd (append (list ack-and-a-half-executable)
                      ack-and-a-half-arguments
                      (ack-and-a-half-arguments-from-options regexp)
                      arguments
                      (list "--" (shell-quote-argument pattern))
                      (when (eq system-type 'windows-nt)
                        (list (concat " < " null-device)))))
         (buf (compilation-start (string-join cmd " ")
                                  'ack-and-a-half-mode
                                  (lambda (&rest _) ack-and-a-half-buffer-name))))
    (when (member 'symbol-overlay features)
      (with-current-buffer buf
        (symbol-overlay-remove-all)
        (setq symbol-overlay-keywords-alist nil)
        (symbol-overlay-put-all pattern nil)))))

(defun ack-and-a-half-read-file (prompt choices)
  (if ido-mode
      (ido-completing-read prompt choices nil t)
    (require 'iswitchb)
    (with-no-warnings
      (let ((iswitchb-make-buflist-hook
             (lambda () (setq iswitchb-temp-buflist choices))))
        (iswitchb-read-buffer prompt nil t)))))

(defun ack-and-a-half-list-files (directory &rest arguments)
  (with-temp-buffer
    (let ((default-directory directory))
      (when (eq 0 (apply 'call-process ack-and-a-half-executable nil t nil "-f" "--print0"
                         arguments))
        (goto-char (point-min))
        (let ((beg (point-min))
              files)
          (while (re-search-forward "\0" nil t)
            (push (buffer-substring beg (match-beginning 0)) files)
            (setq beg (match-end 0)))
          files)))))

(defun ack-and-a-half-version-string ()
  "Return the ack version string."
  (with-temp-buffer
    (call-process ack-and-a-half-executable nil t nil "--version")
    (goto-char (point-min))
    (re-search-forward " +")
    (buffer-substring (point) (line-end-position))))

(defun ack-and-a-half-mode-setup ()
  "Setup compilation variables and buffer for `ack-and-a-half'.
Set up `compilation-exit-message-function'."
  (set (make-local-variable 'compilation-exit-message-function)
       (lambda (status code msg)
         (if (eq status 'exit)
             (cond ((and (zerop code) (buffer-modified-p))
                    '("finished (matches found)\n" . "matched"))
                   ((not (buffer-modified-p))
                    '("finished with no matches found\n" . "no match"))
                   (t
                    (cons msg code)))
           (cons msg code)))))

;;; Public interface ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun ack-and-a-half (pattern &optional regexp directory)
  "Run ack.
PATTERN is interpreted as a regular expression, iff REGEXP is
non-nil.  If called interactively, the value of REGEXP is
determined by `ack-and-a-half-regexp-search'.  A prefix arg
toggles the behavior.  DIRECTORY is the root directory.  If
called interactively, it is determined by
`ack-and-a-half-project-root-file-patterns'.  The user is only
prompted, if `ack-and-a-half-prompt-for-directory' is set."
  (interactive (ack-and-a-half-interactive))
  (ack-and-a-half-run directory regexp pattern))

(defun ack-and-a-half-with-args (pattern &optional regexp directory)
  "Run ack with custom arguments.

PATTERN, REGEXP, and DIRECTORY are interpreted the same way as in
the `ack-and-a-half' function."
  (interactive (ack-and-a-half-interactive))
  (let ((args (read-string "extra args: "
                           nil
                           'ack-and-a-half-extra-args-history)))
    (ack-and-a-half-run directory regexp pattern args)))

;;;###autoload
(defun ack-and-a-half-same (pattern &optional regexp directory)
  "Run ack with --type matching the current `major-mode'.
The types of files searched are determined by
`ack-and-a-half-mode-type-alist' and
`ack-and-a-half-mode-extension-alist'.  If no type is configured,
the buffer's file extension is used for the search.  PATTERN is
interpreted as a regular expression, iff REGEXP is non-nil.  If
called interactively, the value of REGEXP is determined by
`ack-and-a-half-regexp-search'.  A prefix arg toggles that value.
DIRECTORY is the directory in which to start searching.  If
called interactively, it is determined by
`ack-and-a-half-project-root-file-patterns`.  The user is only
prompted, if `ack-and-a-half-prompt-for-directory' is set.`"
  (interactive (ack-and-a-half-interactive))
  (let ((type (ack-and-a-half-type)))
    (if type
        (apply 'ack-and-a-half-run directory regexp pattern type)
      (ack-and-a-half pattern regexp directory))))

;;;###autoload
(defun ack-and-a-half-find-file (&optional directory)
  "Prompt to find a file found by ack in DIRECTORY."
  (interactive (list (ack-and-a-half-read-dir)))
  (find-file (expand-file-name
              (ack-and-a-half-read-file
               "Find file: "
               (ack-and-a-half-list-files directory))
              directory)))

;;;###autoload
(defun ack-and-a-half-find-file-same (&optional directory)
  "Prompt to find a file found by ack in DIRECTORY."
  (interactive (list (ack-and-a-half-read-dir)))
  (find-file (expand-file-name
              (ack-and-a-half-read-file
               "Find file: "
               (apply 'ack-and-a-half-list-files directory (ack-and-a-half-type)))
              directory)))

;;; End ack-and-a-half.el ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'ack-and-a-half)

;;; ack-and-a-half.el ends here
