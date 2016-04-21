;;; marcopolo-mode.el --- Major mode for Docker registry and hub

;; Copyright (C) 2014, 2015, 2016 Nicolas Lamirault <nicolas.lamirault@gmail.com>

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

(require 'widget)
(require 'cl-lib)

(require 's)

(require 'marcopolo-custom)
(require 'marcopolo-hub)
(require 'marcopolo-registry)
(require 'marcopolo-ui)
(require 'marcopolo-utils)


;; Customization

(defgroup marcopolo-mode nil
  "Customization group for `marcopolo-mode'."
  :prefix "marcopolo-mode-"
  :tag "Marcopolo Mode"
  :group 'marcopolo)


(defcustom marcopolo-buffer "*marcopolo*"
  "The Marcopolo buffer name."
  :type 'string
  :group 'marcopolo-mode)

(defcustom marcopolo-padding 2
  "The number of columns used for padding on the left side of the buffer."
  :type 'integer
  :group 'marcopolo-mode)


(defgroup marcopolo-mode-faces '((marcopolo-mode custom-group))
  "Customization group for the faces of `marcopolo-mode'."
  :prefix "marcopolo-mode-"
  :tag "Marcopolo Mode Faces"
  :group 'marcopolo-mode)

(defface marcopolo-title
  '((t :weight bold :inherit font-lock-string-name-face))
  "Face used on the song render in the Marcopolo buffer."
  :group 'marcopolo-mode-faces)

(defface marcopolo-repository-name
  '((t :weight bold :inherit font-lock-string-name-face))
  "Face used on the song render in the Marcopolo buffer."
  :group 'marcopolo-mode-faces)

(defface marcopolo-repository-description
  '((t :inherit font-lock-comment-face))
  "Face used on the song render in the Marcopolo buffer"
  :group 'marcopolo-mode-faces)

(defface marcopolo-repository-misc
  '((t :inherit font-lock-string-face))
  "Face used on the song render in the Dionysos buffer"
  :group 'marcopolo-mode-faces)

;; UI tools


(defun marcopolo--width ()
  "Return the width of the renderable content."
  (- (/ (frame-width) 2) (* marcopolo-padding 2)))


(defun marcopolo--horizontal-rule ()
  "Insert a horizontal rule into the buffer."
  (widget-insert
   (concat (make-string marcopolo-padding ?\s)
	   (make-string (- (marcopolo--width) marcopolo-padding) ?-)
	   (make-string marcopolo-padding ?\s)
	   "\n")))

(defun marcopolo--render-row (left right &optional width-right)
  "Render a row with a `LEFT' and a `RIGHT' part.
Optional argument `WIDTH-RIGHT' is the width of the right argument."
  (let* ((width-right (or width-right (length (or right ""))))
	 (width-left (- (marcopolo--width)
			(- width-right 1)
			(* 2 marcopolo-padding)))
	 (padding (make-string marcopolo-padding ?\s)))
    ;; (widget-insert (format
    ;;     	    (format "%s%%-%s.%ss %%%s.%ss%s\n"
    ;;     		    padding
    ;;     		    width-left width-left
    ;;     		    width-right width-right
    ;;     		    padding)
    ;;     	    left right))))
    (widget-insert
     (format "%s%s\n"
             (s-pad-left (length left) " " left)
             (s-pad-right (length right) " " right)))))

;; Rendering


(defun marcopolo--render-repository (repository)
  "Render a `REPOSITORY' to the Marcopolo buffer."
  (marcopolo--render-row
   (propertize (marcopolo--assoc-cdr 'name repository)
               'face 'marcopolo-repository-name)
   (propertize (marcopolo--assoc-cdr 'is-trusted repository)
               'face 'marcopolo-repository-misc))
  (marcopolo--render-row
    (propertize (s-trim (marcopolo--assoc-cdr 'description repository))
                'face 'marcopolo-repository-description)
    ""))

(defun marcopolo--render-repository-informations (repository)
  "Render all informatiosn about the `REPOSITORY'."
  )


(defun marcopolo--render-repositories (repositories)
  "Render `REPOSITORIES'."
  ;;(message "Repositories: %s" repositories)
  (let ((start (point)))
    (cl-loop
     for n from 1 to (length repositories)
     do (let ((repository (elt repositories (- n 1)))
              (start (point)))
          ;;(message "Repository : %s" repository)
          (marcopolo--render-repository repository)
          (put-text-property start (point) :marcopolo-repository repository)))
    (widget-insert "\n")))



;; Mode


(defun marcopolo-kill-buffer ()
  "Kill the `marcopolo-buffer' and delete the current window."
  (interactive)
  (let ((buffer (get-buffer marcopolo-buffer)))
    (when (equal buffer (current-buffer))
      (delete-window))
    (when buffer
      (kill-buffer buffer))))

(defun marcopolo-describe-repository ()
  "Display informations about the Docker repository at point."
  (let ((repository (marcopolo-current-repository)))
    (when repository
      (marcopolo--render-repository-informations repository))))

(defun marcopolo-current-repository ()
  "Return the current repository at point."
  (get-text-property (point) :marcopolo-repository))


(defun marcopolo-next-repository ()
  "Move point to the next Docker repository."
  (interactive)
  (let ((pos (next-single-property-change (point) :marcopolo-repository)))
    (when pos
      (goto-char pos)
      (unless (marcopolo-current-repository)
	(let ((pos (next-single-property-change pos :marcopolo-repository)))
	  (if pos (goto-char pos)))))))


(defun marcopolo-prev-repository ()
  "Move point to the next Docker repository."
  (interactive)
  (let ((pos (previous-single-property-change (point) :marcopolo-repository)))
    (when pos
      (goto-char pos)
      (unless (marcopolo-current-repository)
	(let ((pos (previous-single-property-change pos :marcopolo-repository)))
	  (if pos (goto-char pos)))))))


(defmacro marcopolo--with-widget (title &rest body)
  `(progn
     (set-buffer (get-buffer-create marcopolo-buffer))
     (switch-to-buffer-other-window marcopolo-buffer)
     (kill-all-local-variables)
     (let ((inhibit-read-only t))
       (erase-buffer)
       (remove-overlays)
       (widget-insert (format "\n%s\n\n" ,title))
       ,@body)
     (use-local-map widget-keymap)
     (widget-setup)
     (marcopolo-mode)
     (widget-minor-mode)
     (goto-char 0)))


(defvar marcopolo-mode-hook nil)

(defvar marcopolo-mode-history nil)


(defvar marcopolo-mode-map
  (let ((map (make-keymap)))
    ;;(define-key map (kbd "i") 'marcopolo-describe-repository)
    (define-key map (kbd "p") 'marcopolo-prev-repository)
    (define-key map (kbd "n") 'marcopolo-next-repository)
    (define-key map (kbd "q") 'marcopolo-kill-buffer)
    map)
  "Keymap for `marcopolo-mode' major mode.")


(define-derived-mode marcopolo-mode tabulated-list-mode
  "Marcopolo mode"
  "Major mode for Marcopolo."
  :group 'marcopolo
  )

;; API

;;;###autoload
(defun marcopolo-registry-search (term)
  "Search from Docker registry repositories using `TERM' request."
  (interactive
   (list (read-from-minibuffer "Search: "
                               (car marcopolo-mode-history)
                               nil
                               nil
                               'marcopolo-mode-history)))
  (marcopolo--with-widget
   (propertize "Docker repositories :")
   (condition-case err
       (marcopolo--render-repositories
        (marcopolo--assoc-cdr 'results (marcopolo--registry-search term)))
     (marcopolo-error
      (message "%s" (error-message-string err))))))


;;;###autoload
(defun marcopolo-hub-search (term)
  "Search from Docker Hub repositories using `TERM' request."
  (interactive
   (list (read-from-minibuffer "Search: "
                               (car marcopolo-mode-history)
                               nil
                               nil
                               'marcopolo-mode-history)))
  (marcopolo--with-widget
   (propertize "Docker repositories :")
   (condition-case err
       (marcopolo--render-repositories
        (marcopolo--assoc-cdr 'results (marcopolo--hub-search term)))
     (marcopolo-error
      (message "%s" (error-message-string err))))))




(provide 'marcopolo-mode)
;;; marcopolo-mode.el ends here
