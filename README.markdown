About
=====

ack-and-a-half.el provides a simple compilation mode for the perl
grep-a-like ack (http://petdance.com/ack/).

Installation
============

Add the following to your .emacs:

    (add-to-list 'load-path "/path/to/ack-and-a-half")
    (require 'ack-and-a-half)
    ;; Create shorter aliases
    (defalias 'ack 'ack-and-a-half)
    (defalias 'ack-same 'ack-and-a-half-same)
    (defalias 'ack-find-file 'ack-and-a-half-find-file)
    (defalias 'ack-find-file-same 'ack-and-a-half-find-file-same)
    (defalias 'ack-with-args 'ack-and-a-half-with-args)

This will load the `ack-and-a-half` functions, and create shorter
aliases for them.

Ignore some directories
=======================

You can specify directories to ignore (`--ignore-dir` parameter of `ack` command)
by customize `ack-and-a-half-ignore-dirs` list.

* To ignore globally `foo` directory, add to your .emacs:
  ```el
  (setq ack-and-a-half-ignore-dirs '("foo"))
  ```

* To ignore `build` directory in `python-mode`:
  ```el
  (add-hook 'python-mode-hook (lambda () (setq ack-and-a-half-ignore-dirs '("build"))))
  ```

* To ignore `foo` directory for a project, in `.dir-locals.el` at the top level of the project:
  ```el
  ((nil . ((ack-and-a-half-ignore-dirs . '("foo")))))
  ```
  or for a specific mode:
  ```el
  ((python-mode . ((ack-and-a-half-ignore-dir . '("foo")))))
  ```

Credits
=======

ack-and-a-half was created from
[ack.el](http://rooijan.za.net/code/emacs-lisp/ack-el) and
[full-ack.el](http://nschum.de/src/emacs/full-ack/).  Both had
features that I liked, but neither was satisfactory on its own.  Thus
`ack-and-a-half` was born.

Contributors
============

Many thanks to the following people for their contributions:

* Alexey Lebedeff
* Andrew Pennebaker
* Andrew Stine
* Derek Chen-Becker
* Gleb Peregud
* Kim van Wyk
* Lars Andersen
* Ronaldo M. Ferraz
* Ryan Thompson
* Simão Mata
