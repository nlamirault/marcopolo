;;; marcopolo-mode.el --- Major mode for Docker registry and hub

;; Copyright (C) 2014, 2015 Nicolas Lamirault <nicolas.lamirault@gmail.com>

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
(require 'marcopolo-ui)

;; Images mode

(defvar marcopolo-registry-image-mode-hook nil)

(defvar marcopolo-registry-image-mode-map
  (let ((map (make-keymap)))
    map)
  "Keymap for `marcopolo-registry-image-mode' major mode.")

(define-derived-mode marcopolo-registry-image-mode tabulated-list-mode
  "Docker registry image mode"
  "Major mode for describing Docker image from registry."
  :group 'marcopolo
  (setq tabulated-list-format [("Entry"  20 t)
                               ("Value"  0 nil)
                               ])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "Entry" nil))
  (tabulated-list-init-header))

(defun marcopolo--create-registry-image-entries (image)
  "Create entries for 'tabulated-list-entries from `IMAGE'."
  (list (list "Created"
              (vector (colorize-term "Created" 'green)
                      (cdr (assoc 'created image))))
        (list "Author"
              (vector (colorize-term "Author" 'green)
                      (cdr (assoc 'author image))))
        (list "OS"
              (vector (colorize-term "OS" 'green)
                      (cdr (assoc 'os image))))
        (list "Arch"
              (vector (colorize-term "Arch" 'green)
                      (cdr (assoc 'architecture image))))
        (list "Docker"
              (vector (colorize-term "Docker" 'green)
                      (cdr (assoc 'docker_version image))))
        (list "ID"
              (vector (colorize-term "ID" 'green)
                      (cdr (assoc 'id image))))
        (list "ID Parent"
              (vector (colorize-term "ID Parent" 'green)
                      (cdr (assoc 'parent image))))
        ))

(defvar marcopolo--registry-image-mode-history nil)

;;;###autoload
(defun marcopolo-registry-describe-image (image)
  "Show Docker repositories  using `IMAGE' request."
  (interactive
   (list (read-from-minibuffer "Image (name:tag): "
                               (car marcopolo--registry-image-mode-history)
                               nil
                               nil
                               'marcopolo--registry-image-mode-history)))
  (pop-to-buffer "*Marcopolo*" nil)
  (let* ((input (s-split ":" image))
         (repo (s-split "/" (car input))))
    (setq tabulated-list-entries
          (marcopolo--create-registry-image-entries
           (marcopolo--registry-image-layer
            (marcopolo--registry-repository-tag-imageid (car repo)
                                                        (cadr repo)
                                                        (cadr input))))))
  (tabulated-list-print t))


;; Repositoriy tags mode

(defvar marcopolo-registry-tag-mode-hook nil)

(defvar marcopolo-registry-tag-mode-map
  (let ((map (make-keymap)))
    map)
  "Keymap for `marcopolo-registry-tag-mode' major mode.")

(define-derived-mode marcopolo-registry-tag-mode tabulated-list-mode
  "Docker registry tag mode"
  "Major mode for describing Docker tag from registry."
  :group 'marcopolo
  (setq tabulated-list-format [("Name"  20 t)
                               ("ID"  0 nil)
                               ])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "Name" nil))
  (tabulated-list-init-header))

(defun marcopolo--create-registry-repository-tags-entries (tags)
  "Create entries for 'tabulated-list-entries from `TAGS'."
  (mapcar (lambda (tag)
            (let ((name (format "%s" (cdar tag))))
              (list name
                    (vector (colorize-term name 'green) (cdadr tag)))))
          tags))

(defvar marcopolo--repository-tag-mode-history nil)

;;;###autoload
(defun marcopolo-registry-repository-tags (repo)
  "Show Docker repositories  using `REPO' request."
  (interactive
   (list (read-from-minibuffer "Repository: "
                               (car marcopolo--repository-tag-mode-history)
                               nil
                               nil
                               'marcopolo--repository-tag-mode-history)))
  (pop-to-buffer "*Marcopolo*" nil)
  (marcopolo-registry-tag-mode)
  (let ((input (s-split "/" repo)))
    (setq tabulated-list-entries
          (marcopolo--create-registry-repository-tags-entries
           (marcopolo--registry-repositories-tags (car input) (cadr input)))))
  (tabulated-list-print t))


;; Repositories mode

(defvar marcopolo-registry-repositories-mode-hook nil)

(defvar marcopolo-registry-repositories-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "v") 'marcopolo--describe-image)
    (define-key map (kbd "d") 'marcopolo--display-image)
    map)
  "Keymap for `marcopolo-registry-repositories-mode' major mode.")

(define-derived-mode marcopolo-registry-repositories-mode tabulated-list-mode
  "Docker registry repositories mode"
  "Major mode for browsing Docker registry repositories."
  :group 'marcopolo
  (setq tabulated-list-format [("Name"  35 t)
                               ("Description"  0 nil)
                               ])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "Name" nil))
  (tabulated-list-init-header))

(defvar marcopolo--repositories-mode-history nil)

(defun marcopolo--create-registry-search-entries (repositories)
  "Create entries for 'tabulated-list-entries from `REPOSITORIES'."
  (mapcar (lambda (result)
            (let ((name (cdr (assoc 'name result))))
              (list name
                    (vector (colorize-term name 'green)
                            (let ((desc (cdr (assoc 'description result))))
                              (if desc
                                  desc
                                ""))))))
          (cdr (fourth repositories))))

;;;###autoload
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
