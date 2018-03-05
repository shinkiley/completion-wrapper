;;; completion-wrapper.el ---
;;
;; Author:  <shinki@yui-pc>
;; Version: 0.1
;; URL: 
;; Keywords:
;; Compatibility:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;; .
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TODO:
;; - add emacs completion
;;
;;; Code:

(require 'seq)
(require 'cl-lib)

(require 'emacs-ef)

(ef-prefixied cw completion-wrapper
  (defvar-cw- checkable-completers '((helm (lambda ()
                                             ;; todo: move to `($expand-function package-is-installed)'
                                             (completion-wrapper--package-is-installed-p 'helm)))
                                     (first (lambda ()
                                              t))))

  (defvar-cw default-completer 'helm
    "Default completer that will be used in functions defined below.")

  (defun-cw complete (candidates)
    "Invoke completion engine with CANDIDATES and return completion in the case of the success.
Otherwise, return nil."
    ($@- check-candidates-are-valid candidates)
    (if (eq (length candidates)
            1)
        (car candidates)
      ($@- get-completion-result candidates)))

  (defun-cw- check-candidates-are-valid (candidates)
    "Raises an error if candidates are not valid."
    (when (eq (length candidates)
              0)
      (error "No candidates was passed in complete function")))

  (defun-cw- get-completion-result (candidates) 
    (let ((completer ($@- get-completer ($? default-completer))))
      (cond
       (($@- is-helm-p completer)
        ($@- complete-helm candidates))

       (($@- is-first-p completer)
        ($@- complete-first candidates))

       (t
        (error "Invalid completion engine in use")))))

  (defun-cw- get-completer (preferred-completer)
    (let ((completers ($@- find-completers)))
      (if (cl-member preferred-completer
                     completers)
          preferred-completer
        (car completers))))

  (defun-cw- find-completers ()
    "Return list of available completers"
    (cl-mapcar #'car
               (seq-filter (lambda (completer)
                             (funcall (cadr completer)))
                           ($?- checkable-completers))))

  (defun-cw- package-is-installed-p (package)
    "Return t if PACKAGE is installed. It loades the package."
    (if (symbolp package)
        (require package nil 'noerror)
      nil))

  (defun-cw- is-helm-p (completer)
    "Return t if selected completer is helm"
    (equal completer
           'helm))

  (defun-cw- complete-helm (candidates)
    "Get user choice using helm."
    (helm (helm-build-sync-source "test"
            :candidates candidates)))

  (defun-cw- is-first-p (completer)
    (equal completer
           'first))

  (defun-cw- complete-first (candidates)
    (if (> (length candidates)
           0)
        (car candidates)
      (error "No candidates for completion")))
  )

(provide 'completion-wrapper)
;;; completion-wrapper.el ends here
