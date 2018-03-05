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

(require 'cl-lib)
(require 'emacs-ef)

(ef-prefixied cw completion-wrapper
  (defconst-cw- checkable-completers '(helm))

  (defun-cw- package-is-installed (package)
    "Return t if PACKAGE is installed. It loades the package."
    (if (symbolp package)
        (require package nil 'noerror)
      nil))

  (defun-cw- find-completers ()
    "Return list of available completers"
    (seq-filter (lambda (item)
                  ($@- package-is-installed item))
                ($?- checkable-completers)))

  (defconst-cw completers ($@- find-completers)
    "Available completion engines.")

  (defun-cw- get-completer (&optional preferred-completer)
    (cond
     (($@- package-is-installed preferred-completer)
      preferred-completer)

     ((> (length ($? completers))
         0)
      (car ($? completers)))

     (t (error "No completers available"))))

  (defvar-cw default-completer ($@- get-completer 'helm)
    "Default completer that will be used in functions defined below.")

  (defun-cw complete (candidates)
    "Invoke completion engine with CANDIDATES and return completion in the case of the success.
Otherwise, return nil."
    (cond
     (($@- default-is-helm-p)
      ($@- complete-helm candidates))

     (t
      (error "Invalid completion engine in use"))))

  (defun-cw- default-is-helm-p ()
    "Return t if selected completed is p"
    (equal ($? default-completer)
           'helm))

  (defun-cw- complete-helm (candidates)
    "Get user choice using helm."
    (if ($@- package-is-installed 'helm)
        (helm (helm-build-sync-source "test"
                :candidates candidates))
      (error "helm is not installed")))
  )

(provide 'completion-wrapper)
;;; completion-wrapper.el ends here
