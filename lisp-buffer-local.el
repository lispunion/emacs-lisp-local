;;; lisp-buffer-local.el --- Configure custom Lisp/Scheme indentation per each file -*- lexical-binding: t -*-
;;
;; SPDX-License-Identifier: ISC
;; Author: Lassi Kortela <lassi@lassi.io>
;; URL: https://github.com/lassik/emacs-lisp-buffer-local
;; Package-Requires: ((emacs "24.3") (cl-lib "0.5"))
;; Package-Version: 0.1.0
;; Keywords: languages lisp
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Configure custom Lisp/Scheme indentation per each file.
;;
;;; Code:

(defvar-local lisp-buffer-local-symbol-properties nil
  "Buffer-local Lisp indentation properties.

List of (SYMBOL . PLIST) sublists. Easiest to show by example:

((when lisp-indent-function 1)
 (if scheme-indent-function 1))")

(defvar-local lisp-buffer-local--real-lisp-indent-function nil
  "")

(defun lisp-buffer-local--valid-plists-p (plists)
  (and (listp plists)
       (cl-every (lambda (sym-plist)
                   (and (listp sym-plist)
                        (symbolp (car sym-plist))
                        (let ((plist (cdr sym-plist)))
                          (while (and (consp plist)
                                      (symbolp (car plist))
                                      (consp (cdr plist)))
                            (setq plist (cddr plist)))
                          (null plist))))
                 plists)))

(defun lisp-buffer-local--call-with-properties (fun &rest args)
  (cond ((not (lisp-buffer-local--valid-plists-p
               lisp-buffer-local-symbol-properties))
         (message
          "Warning: ignoring invalid lisp-buffer-local-symbol-properties")
         (apply fun args))
        (t
         (let* ((new-plists lisp-buffer-local-symbol-properties)
                (old-plists
                 (mapcar (lambda (sym) (cons sym (symbol-plist sym)))
                         (mapcar #'car new-plists))))
           (mapc (lambda (sym-plist)
                   (setplist (car sym-plist) (cdr sym-plist)))
                 new-plists)
           (unwind-protect (apply fun args)
             (mapc (lambda (sym-plist)
                     (setplist (car sym-plist) (cdr sym-plist)))
                   old-plists))))))

(defun lisp-buffer-local--lisp-indent-function (&rest args)
  (cl-assert lisp-buffer-local--real-lisp-indent-function)
  (apply #'lisp-buffer-local--call-with-properties
         lisp-buffer-local--real-lisp-indent-function
         args))

(defun lisp-buffer-local ()
  (setq-local lisp-buffer-local--real-lisp-indent-function
              (or lisp-buffer-local--real-lisp-indent-function
                  lisp-indent-function))
  (setq-local lisp-indent-function
              #'lisp-buffer-local--lisp-indent-function))

(provide 'lisp-buffer-local)

;;; lisp-buffer-local.el ends here
