;;; lisp-indent-buffer-local.el --- Configure custom Lisp/Scheme indentation per each file -*- lexical-binding: t -*-
;;
;; SPDX-License-Identifier: ISC
;; Author: Lassi Kortela <lassi@lassi.io>
;; URL: https://github.com/lassik/emacs-lisp-indent-buffer-local
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

(defvar-local lisp-indent-buffer-local nil
  "Buffer-local Lisp indentation properties.

List of (SYMBOL . PLIST) sublists. Easiest to show by example:

((when lisp-indent-function 1)
 (if scheme-indent-function 1))")

(defvar-local lisp-indent-buffer-local--orig-fun nil)

(defun lisp-indent-buffer--local-valid-p (plists)
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

(defun lisp-indent-buffer-local--advice (fun &rest args)
  (cond ((not (lisp-indent-buffer--local-valid-p lisp-indent-buffer-local))
         (message "Warning: ignoring invalid lisp-indent-buffer-local")
         (apply fun args))
        (t
         (let* ((new-plists lisp-indent-buffer-local)
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

(defun lisp-indent-buffer-local--indent-function (&rest args)
  (cl-assert lisp-indent-buffer-local--orig-fun)
  (apply #'lisp-indent-buffer-local--advice
         lisp-indent-buffer-local--orig-fun
         args))

(defun lisp-indent-buffer-local ()
  (setq-local lisp-indent-buffer-local--orig-fun
              (or lisp-indent-buffer-local--orig-fun
                  lisp-indent-function))
  (setq-local lisp-indent-function
              #'lisp-indent-buffer-local--indent-function))

(provide 'lisp-indent-buffer-local)

;;; lisp-indent-buffer-local.el ends here
