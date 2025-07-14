;;; ack-and-a-half.el --- Yet another front-end for ack -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2009 Nikolaj Schumacher
;; Copyright (C) 2013 Jacob Helwig <jacob@technosorcery.net>
;; Copyright (C) 2013-2025 Various contributors
;; Copyright (C) 2025 Alain Delplanque
;;
;; Author: Jacob Helwig <jacob+ack@technosorcery.net>
;; Maintainer: Alain Delplanque <alaindelplanque@mailoo.org>
;; URL: https://github.com/adelplanque/ack-and-a-half
;; Version: 1.2.0
;; Package-Requires: ((emacs "25.1"))
;; Keywords: tools, search, ack
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; ack-and-a-half.el provides a simple compilation mode for the Perl grep-like
;; tool "ack" (https://beyondgrep.com/).
;;
;; Add the following to your init file:
;;
;;     (add-to-list 'load-path "/path/to/ack-and-a-half")
;;     (require 'ack-and-a-half)
;;     (defalias 'ack 'ack-and-a-half)
;;     (defalias 'ack-same 'ack-and-a-half-same)
;;     (defalias 'ack-find-file 'ack-and-a-half-find-file)
;;     (defalias 'ack-find-file-same 'ack-and-a-half-find-file-same)
;;
;; Run `ack-and-a-half' to search all files, or `ack-and-a-half-same' to search
;; only files of the same type as the current buffer.  Use `next-error' /
;; `previous-error' to navigate between results.
;;
;; `ack-find-file' and `ack-find-file-same' use ack to list project files,
;; useful as a file navigator.
;;
;; ----
;;
;; This package is a maintained fork of `ack-and-a-half.el`, originally authored
;; by Jacob Helwig and contributors.  That version was itself a near-complete
;; rewrite of `full-ack.el` by Nikolaj Schumacher (GPLv2+).
;;
;; The fork appears to have diverged informally before public Git history
;; (pre-2009), without formal license continuity.  This version restores GPL
;; licensing and historical attribution accordingly.

;;; Code:

(require 'compile)
(require 'eieio)
(require 'grep)
(require 'symbol-overlay nil t)
(require 'thingatpt)

(define-compilation-mode ack-and-a-half-mode "Ack"
  "Major mode for viewing ack search results."
  (setq-local truncate-lines t)
  (setq-local compilation-disable-input t)
  (setq-local compilation-process-setup-function #'ack-and-a-half-mode-setup)
  (setq-local compilation-error-face grep-hit-face)
  (let ((smbl  'compilation-ack-nogroup)
        (pttrn '("^\\([^:\n]+?\\):\\([0-9]+\\):\\([0-9]+\\):" 1 2 3)))
    (setq-local compilation-error-regexp-alist (list smbl))
    (setq-local compilation-error-regexp-alist-alist (list (cons smbl pttrn)))))

(defgroup ack-and-a-half nil "Yet another front end for ack."
  :group 'tools
  :group 'matching)

(defcustom ack-and-a-half-executable-ack (or (executable-find "ack")
                                             (executable-find "ack-grep"))
  "*The location of the ack executable."
  :group 'ack-and-a-half
  :type 'file)

(defcustom ack-and-a-half-executable-ripgrep (or (executable-find "rg")
                                                 (executable-find "ripgrep"))
  "*The location of the ripgrep executable."
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
This overrides values in `ack-and-a-half--mode-type-default-alist'.
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

(defcustom ack-and-a-half-default-same t
  "Flag to limit search to files of the same type."
  :group 'ack-and-a-half
  :type '(choice (const :tag "Search for file of same type" nil)
                 (const :tag "Search for all files" t)))

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

(defcustom ack-and-a-half-ignore-dirs nil
  "List of directories to be ignored by ack command.

This will be append as `--ignore-dir' parameters of the `ack'
commande.  Variable is Buffer-Local and can be customized
differently depending on the mode."
  :group 'ack-and-a-half
  :type '(repeat string))
(make-variable-buffer-local 'ack-and-a-half-ignore-dirs)

;;; Default setting lists ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst ack-and-a-half--mode-type-default-alist
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

(defvar ack-and-a-half--pattern-history nil
  "Strings recently searched for with `ack-and-a-half'.")
(defvar ack-and-a-half--extra-args-history nil
  "Extra args recents passed to ack with `ack-and-a-half-with-args'.")

(defun ack-and-a-half--default-pattern ()
  "Return the initial search pattern.
Return the active region if it exists, otherwise the symbol at point."
  (cond
   ((use-region-p)
    (buffer-substring-no-properties (region-beginning) (region-end)))
   ((when-let ((sym (thing-at-point 'symbol)))
      (set-text-properties 0 (length sym) nil sym)
      sym))
   (t "")))

(defun ack-and-a-half--root-directory ()
  "Determine root projet directory from which search must begin."
  (run-hook-with-args-until-success 'ack-and-a-half-root-directory-functions))

(defun ack-and-a-half-option (name enabled)
  "Format cli flag --name/--noname according to NAME and boolean ENABLED."
  (format "--%s%s" (if enabled "" "no") name))

(defclass ack-and-a-half--backend ()
  ((name :initarg :name
         :initform nil
         :documentation "Backend name.")
   (mode-type-alist :initarg :mode-type-alist
                    :initform nil
                    :documentation
                    "major-mode type mapping for --type argument.")))

(defun ack-and-a-half-create-type (extensions)
  "Create ack options to filter files by EXTENSIONS."
  (list "--type" "customtype" "--type-set"
        (concat "customtype:ext:" (mapconcat 'identity extensions ","))))

(cl-defmethod ack-and-a-half--backend-type-args ((backend ack-and-a-half--backend))
  "Return type-related command-line arguments for BACKEND."
  (let ((types (cdr (or (assoc major-mode ack-and-a-half-mode-type-alist)
                        (assoc major-mode (oref backend mode-type-alist)))))
        (ext (cdr (or (assoc major-mode ack-and-a-half-mode-extension-alist)
                      (assoc major-mode ack-and-a-half-mode-extension-default-alist))))
        result)
    (unless (or types ext)
      (when buffer-file-name
        (setq ext (list (file-name-extension buffer-file-name)))))
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

(defun ack-and-a-half--setup-args (args)
  "Fill missing argument in ARGS plist and return a new one."
  (list :directory (or (plist-get args :directory) (ack-and-a-half--root-directory))
        :regexp (if (plist-member args :regexp)
                    (plist-get args :regexp)
                  ack-and-a-half-regexp-search)
        :same (if (plist-member args :same)
                  (plist-get args :same)
                ack-and-a-half-default-same)
        :extra-args (or (plist-get args :extra-args) "")
        :ignore-dirs (or (plist-get args :ignore-dirs)
                         ack-and-a-half-ignore-dirs)))

(cl-defmethod ack-and-a-half--backend-run ((backend ack-and-a-half--backend)
                                           pattern args)
  "Run BACKEND to search for PATTERN according to ARGS."
  (let* ((args (ack-and-a-half--setup-args args))
         (default-directory (plist-get args :directory))
         (cmd-list (ack-and-a-half--backend-get-cmd backend pattern args))
         (cmd (mapconcat #'shell-quote-argument cmd-list " "))
         (buf (compilation-start cmd 'ack-and-a-half-mode
                                 (lambda (&rest _) ack-and-a-half-buffer-name))))
    (when (member 'symbol-overlay features)
      (with-current-buffer buf
        (symbol-overlay-remove-all)
        (setq symbol-overlay-keywords-alist nil)
        (symbol-overlay-put-all pattern nil)))))

(defclass ack-and-a-half--backend-ack (ack-and-a-half--backend)
  ((mode-type-alist :initarg :mode-type-alist
                    :initform (append '((python-mode "python"))
                                      ack-and-a-half--mode-type-default-alist)))
  "Concrete backend class for ack.")

(cl-defmethod ack-and-a-half--backend-get-cmd ((backend ack-and-a-half--backend-ack)
                                               pattern args)
  "Return a list of command-line arguments for ack BACKEND.
This will search for PATTERN using the options in ARGS."
  (append (list ack-and-a-half-executable-ack
                "--sort-files" "--nocolor" "--nogroup" "--column"
                (ack-and-a-half-option "smart-case" (eq ack-and-a-half-ignore-case 'smart))
                (ack-and-a-half-option "env" ack-and-a-half-use-environment))
          (when (plist-get args :same) (ack-and-a-half--backend-type-args backend))
          (cl-mapcan (lambda (x) (list "--ignore-dir" x)) (plist-get args :ignore-dirs))
          (unless ack-and-a-half-ignore-case '("-i"))
          (unless (plist-get args :regexp) '("--literal"))
          ack-and-a-half-arguments
          (split-string-and-unquote (plist-get args :extra-args))
          (list "--" pattern)
          (when (eq system-type 'windows-nt)
            (list (concat " < " null-device)))))

(defclass ack-and-a-half--backend-ripgrep (ack-and-a-half--backend)
  ((mode-type-alist :initarg :mode-type-alist
                    :initform (append '((python-mode "py"))
                                      ack-and-a-half--mode-type-default-alist)))
  "Concrete backend class for ripgrep.")

(cl-defmethod ack-and-a-half--backend-get-cmd ((backend ack-and-a-half--backend-ripgrep)
                                               pattern args)
  "Return a list of command-line arguments for ripgrep BACKEND.
This will search for PATTERN using the options in ARGS."
  (append (list ack-and-a-half-executable-ripgrep
                "--no-heading" "--sort=path" "--color=never" "--column"
                (ack-and-a-half-option "smart-case" (eq ack-and-a-half-ignore-case 'smart)))
          (when (plist-get args :same) (ack-and-a-half--backend-type-args backend))
          (cl-mapcan (lambda (x) (list "--glob" (format "!**/%s/**" x)))
                     (plist-get args :ignore-dirs))
          (unless (plist-get args :regexp) '("--fixed-strings"))
          (list "--" pattern)))

(defvar ack-and-a-half--backends
  (list (ack-and-a-half--backend-ack :name "ack")
        (ack-and-a-half--backend-ripgrep :name "ripgrep"))
  "List of available backend instances.")

(defun ack-and-a-half-version-string ()
  "Return the ack version string."
  (with-temp-buffer
    (call-process ack-and-a-half-executable-ack nil t nil "--version")
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

(defclass ack-and-a-half--option ()
  ((state :initarg :state
          :initform nil
          :documentation "Courrent value.")
   (descr :initarg :descr
          :initform nil)
   (key :initarg :key
        :initform nil))
  "Search parameter.")

(defclass ack-and-a-half--option-choices (ack-and-a-half--option)
  ((choices :initarg :choices
            :initform nil
            :type list
            :documentation "List of available cases."))
  "Multiple choice option.")

(defclass ack-and-a-half--option-dir (ack-and-a-half--option) nil
  "Directory type option.")

(defclass ack-and-a-half--option-text (ack-and-a-half--option) nil
  "Free text type option.")

(cl-defmethod ack-and-a-half--option-hit ((opt ack-and-a-half--option-choices))
  "Manager when the choice option OPT is enabled."
  (let* ((choices (oref opt choices))
         (current (oref opt state)))
    (oset opt state (or (cadr (member current choices))
                        (car choices)))))

(cl-defmethod ack-and-a-half--option-hit ((opt ack-and-a-half--option-dir))
  "Manager when the directory option OPT is enabled."
  (let* ((enable-recursive-minibuffers t)
         (minibuffer-setup-hook
          (append minibuffer-setup-hook
                  (list (lambda () (setq-local enable-recursive-minibuffers nil)))))
         (new-dir (if (and (boundp 'ido-mode) ido-mode)
                      (ido-read-directory-name (format "%s: " (oref opt descr))
                                               (oref opt state))
                    (read-directory-name (format "%s: " (oref opt descr))
                                         (oref opt state)))))
    (oset opt state new-dir)))

(cl-defmethod ack-and-a-half--option-hit ((opt ack-and-a-half--option-text))
  "Manager when the text option OPT is enabled."
  (let* ((enable-recursive-minibuffers t)
         (minibuffer-setup-hook
          (append minibuffer-setup-hook
                  (list (lambda () (setq-local enable-recursive-minibuffers nil)))))
         (txt (read-from-minibuffer (format "%s: " (oref opt descr)) nil nil nil
                                    'ack-and-a-half--extra-args-history
                                    (oref opt state))))
    (oset opt state txt)))

(cl-defmethod ack-and-a-half--option-format-choice ((opt ack-and-a-half--option-choices) value)
  "Format the VALUE according to the OPT choice option.

It will be highlighted when it matches the current value."
  (if (string= value (oref opt state))
      (propertize value 'face 'highlight)
    (propertize value 'face 'shadow)))

(cl-defmethod ack-and-a-half--option-format ((opt ack-and-a-half--option))
  "Format an option OPT."
  (format "%s (%s) [%s]" (oref opt descr) (oref opt key) (oref opt state)))

(cl-defmethod ack-and-a-half--option-format ((opt ack-and-a-half--option-choices))
  "Format a choice option OPT."
  (format "%s (%s) [%s]" (oref opt descr) (oref opt key)
          (mapconcat (lambda (v) (ack-and-a-half--option-format-choice opt v))
                     (oref opt choices)
                     "|")))

(defun ack-and-a-half--options-display (buf)
  "Displays the contents of the options buffer BUF."
  (with-current-buffer buf
    (erase-buffer)
    (when (boundp 'ack-and-a-half--options)
      (insert (mapconcat #'ack-and-a-half--option-format ack-and-a-half--options " - ")))
    (goto-char (point-min))))

(defun ack-and-a-half--options-buffer (options)
  "Builds the option buffer with the OPTIONS list.

The buffer is displayed immediately above the minibuffer.
Returns the newly created buffer."
  (let ((buf (get-buffer-create "*Ack options*")))
    (with-current-buffer buf
      (setq-local ack-and-a-half--options options))
    (ack-and-a-half--options-display buf)
    (display-buffer buf '((display-buffer-at-bottom) (window-height . 2)))
    buf))

(defun ack-and-a-half--interactive-args ()
  "Determine the parameters of the ack search in interactive mode."
  (let* ((backend (ack-and-a-half--option-choices
                   :choices '("ack" "ripgrep")
                   :state "ack"
                   :key "C-a"
                   :descr "Backend"))
         (regexp (ack-and-a-half--option-choices
                  :choices '("yes" "no")
                  :state (if ack-and-a-half-regexp-search "yes" "no")
                  :key "C-r"
                  :descr "Regex"))
         (same (ack-and-a-half--option-choices
                :choices '("yes" "no")
                :state (if ack-and-a-half-default-same "yes" "no")
                :key "C-t"
                :descr "Same"))
         (directory (ack-and-a-half--option-dir
                     :state (ack-and-a-half--root-directory)
                     :key "C-d"
                     :descr "Dir"))
         (ignore-dirs (ack-and-a-half--option-text
                       :state (mapconcat #'identity ack-and-a-half-ignore-dirs ":")
                       :key "C-i"
                       :descr "Ignore"))
         (extra-args (ack-and-a-half--option-text
                      :state ""
                      :key "C-e"
                      :descr "Args"))
         (options (list backend same regexp directory ignore-dirs extra-args))
         (buf (ack-and-a-half--options-buffer options))
         (map (copy-keymap minibuffer-local-map)))
    (unwind-protect
        (progn
          (dolist (opt options)
            (define-key map (kbd (oref opt key))
                        (lambda () (interactive)
                          (ack-and-a-half--option-hit opt)
                          (ack-and-a-half--options-display buf))))
          (let* ((default (ack-and-a-half--default-pattern))
                 (pattern (read-from-minibuffer
                           (if default (format "Ack (default %s): " default) "Ack: ")
                           nil map nil 'ack-and-a-half--history default)))
            (with-current-buffer buf
              (list (if (string-blank-p pattern) default pattern)
                    :backend (oref backend state)
                    :directory (oref directory state)
                    :extra-args (oref extra-args state)
                    :ignore-dirs (let ((dirs (oref ignore-dirs state)))
                                   (when (not (string= dirs "")) (split-string dirs ":")))
                    :regexp (if (string= (oref regexp state) "yes") t nil)
                    :same (if (string= (oref same state) "yes") t nil)))))
      (when (buffer-live-p buf)
        (let ((win (get-buffer-window buf)))
          (when (window-live-p win) (delete-window win)))
        (kill-buffer buf)))))

;;;###autoload
(defun ack-and-a-half (pattern &rest args)
  "Main function to search for a PATTERN on a set of files.

The search can be refined according to the ARGS arguments plist.
`:directory' Directory to search in
`:ignore-dirs' List of directories to be ignored
`:extra-args' Arbitrary arguments to pass to the command (string)
`:regexp' Literal search (nil) or pattern search (t)
`:same' Search among files of the same type as the current buffer (t/nil)

In interactive mode, the user is prompted for the expression to search for in
the minibuffer.  The search parameters are displayed just above and can be
refined using keyboard shortcuts."
  (interactive (ack-and-a-half--interactive-args))
  (let* ((backend-name (or (plist-get args :backend)
                           (oref (car ack-and-a-half--backends) name)))
         (backend (seq-find (lambda (backend)
                              (equal (oref backend name) backend-name))
                            ack-and-a-half--backends)))
    (ack-and-a-half--backend-run backend pattern args)))

(provide 'ack-and-a-half)

;;; ack-and-a-half.el ends here
