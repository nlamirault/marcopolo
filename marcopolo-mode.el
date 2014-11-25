;;; marcopolo-mode.el --- Major mode for Docker registry and hub

;; Copyright (C) 2014 Nicolas Lamirault <nicolas.lamirault@gmail.com>

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301, USA.

;;; Commentary:

;;; Code:

(require 'cl-lib)
(require 'tabulated-list)

;; Travis library

(require 'marcopolo-registry)

;; Repositories mode

(defvar marcopolo-registry-repositories-mode-hook nil)

(defvar marcopolo-registry-repositories-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "w") 'marcopolo--registry-status)
    map)
  "Keymap for `marcopolo-registry-repositories-mode' major mode.")

(define-derived-mode marcopolo-registry-repositories-mode tabulated-list-mode
  "Docker registry repositories mode"
  "Major mode for browsing Docker registry repositories."
  :group 'marcopolo
  (setq tabulated-list-format [("Name"  25 t)
                               ("Description"  0 nil)
                               ])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "Name" nil))
  (tabulated-list-init-header))

;; Repositories data

(defvar marcopolo--repositories-mode-history nil)

(defun marcopolo--create-registry-search-entries (repositories)
  "Create entries for 'tabulated-list-entries from `REPOSITORIES'."
  (mapcar (lambda (result)
            (let ((name (cdr (assoc 'name result))))
              (list name
                    (vector name ;(colorize-build-state name)
                            (cdr (assoc 'description result))))))
          (cdar repositories)))

(defun marcopolo-registry-search (term)
  "Show Docker repositories  using `TERM' request."
  (interactive
   (list (read-from-minibuffer "Search: "
                               (car marcopolo--repositories-mode-history)
                               nil
                               nil
                               'marcopolo--repositories-mode-history)))
  (pop-to-buffer "*Marcopolo*" nil)
  (marcopolo-registry-repositories-mode)
  (setq tabulated-list-entries
        (marcopolo--create-registry-search-entries
         (marcopolo--registry-search term)))
  (tabulated-list-print t))





(provide 'marcopolo-mode)
;;; marcopolo-mode.el ends here
