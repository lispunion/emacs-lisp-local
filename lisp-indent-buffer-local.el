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
  "Bla bla")

(defvar lisp-indent-buffer-local-functions
  '(lisp-indent-function
    common-lisp-indent-function
    scheme-indent-function
    clojure-indent-function))

(defvar lisp-indent-buffer-local-get-builtin (symbol-function 'get)
  "")

(defun lisp-indent-buffer-local-get (symbol propname)
  (message "getting %S property %S" symbol propname)
  (let* ((props (and (listp lisp-indent-buffer-local)
                     (assoc symbol lisp-indent-buffer-local)))
         (prop  (and (listp props) (assoc propname props))))
    (if prop (cdr prop)
        (let ((builtin lisp-indent-buffer-local-get-builtin))
          (unless (and (subrp builtin) (equal "get" (subr-name builtin)))
            (error "Unable to override built-in `get' function"))
          (funcall builtin symbol propname)))))

(defun lisp-indent-buffer-local-advice (fun &rest args)
  (let ((old-get (symbol-function 'get)))
    (setf (symbol-function 'get) #'lisp-indent-buffer-local-get)
    (message "get is now %S" (symbol-function 'get))
    (unwind-protect (apply fun args)
      (setf (symbol-function 'get) old-get))))

(defun lisp-indent-buffer-local-enable ()
  (dolist (symbol lisp-indent-buffer-local-functions)
    (add-function :around (symbol-function symbol)
                  #'lisp-indent-buffer-local-advice
                  '((name . lisp-indent-buffer-local-advice)))))

(defun lisp-indent-buffer-local-disable ()
  (dolist (symbol lisp-indent-buffer-local-functions)
    (remove-function (symbol-function symbol)
                     'lisp-indent-buffer-local-advice)))

(provide 'lisp-indent-buffer-local)

;;; lisp-indent-buffer-local.el ends here
