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

(require 'widget)
(require 'cl-lib)

(require 's)

(require 'marcopolo-custom)
(require 'marcopolo-registry)
(require 'marcopolo-ui)


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

(defvar marcopolo-mode--repository-buffer nil
  "Buffer being used to display repositiry informations.")

(defvar marcopolo-mode--docker-site nil
  "Buffer local Docker site : 'hub or 'registry.")


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
    (widget-insert (format
        	    (format "%s%%-%s.%ss %%%s.%ss%s\n"
        		    padding
        		    width-left width-left
        		    width-right width-right
        		    padding)
        	    left right))))
    ;; (widget-insert
    ;;  (format "%s%s\n"
    ;;          (s-pad-left (length left) " " left)
    ;;          (s-pad-right (length right) " " right)))))


(defun marcopolo--create-repository-window ()
  "Create a new window for describing a Docker repository."
  (let ((window
         (condition-case er
             (split-window (selected-window) 10 'below)
           (error
            ;; If the window is too small to split, use any one.
            (if (string-match
                 "Window #<window .*> too small for splitting"
                 (car (cdr-safe er)))
                (next-window)
              (error (cdr er)))))))
    ;; Configure the window to be closed on `q'.
    (set-window-prev-buffers window nil)
    (set-window-parameter window 'quit-restore
                          ;; See (info "(elisp) Window Parameters")
                          `(window window ,(selected-window) ,marcopolo-mode--repository-buffer))
    window))



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
  "Render a `REPOSITORY' informations to the repository buffer."
  (message "Docker registry Site: %s" marcopolo-mode--docker-site)
  (marcopolo--render-row
   (propertize (marcopolo--assoc-cdr 'name repository)
               'face 'marcopolo-repository-name)
   (propertize (marcopolo--assoc-cdr 'is-trusted repository)
               'face 'marcopolo-repository-misc))
  (marcopolo--render-row
    (propertize (s-trim (marcopolo--assoc-cdr 'description repository))
                'face 'marcopolo-repository-description)
    "")
  (let* ((infos (s-split "/" (marcopolo--assoc-cdr 'name repository)))
         (site marcopolo-mode--docker-site)
         (tags (marcopolo-repository-tags (car infos) (cadr infos) site))
         (images (marcopolo-repository-images (car infos) (cadr infos) site)))
    (marcopolo--render-row "\nTags:\n" "")
    (mapc (lambda (tag)
            (message "Tag: %s" tag)
            (if (eql 'registry site)
                (marcopolo--render-row
                 (propertize (symbol-name (car tag)) 'face 'marcopolo-repository-misc)
                 (cdr tag))
              (marcopolo--render-row
               (propertize (cdar tag) 'face 'marcopolo-repository-misc)
               (cdadr tag))))
          tags)
    (marcopolo--render-row "\nImages:\n" "")
    (mapc (lambda (image)
            (message "Image: %s" image)
            (marcopolo--render-row
             (propertize (cdar image) 'face 'marcopolo-repository-misc)
             ""))
          images)))

(defun marcopolo--render-repositories (repositories)
  "Render `REPOSITORIES'."
  ;;(message "Repositories: %s" repositories)
  (message "Site: %s" marcopolo-mode--docker-site)
  (let ((start (point)))
    (cl-loop
     for n from 1 to (length repositories)
     do (let ((repository (elt repositories (- n 1)))
              (start (point)))
          ;;(message "Repository : %s" repository)
          (marcopolo--render-repository repository)
          (put-text-property start (point) :marcopolo-repository repository)))
    (widget-insert "\n")))


(defun marcopolo-mode--display-repository-buffer (window)
  "Display and return the buffer used for displaying a question.
Create `marcopolo-mode--repository-buffer' if necessary.
If WINDOW is given, use that to display the buffer."
  ;; Create the buffer if necessary.
  (unless (buffer-live-p marcopolo-mode--repository-buffer)
    (setq marcopolo-mode--repository-buffer
          (generate-new-buffer "*marcopolo-repository*")))
  (cond
   ;; Window was given, use it.
   ((window-live-p window)
    (set-window-buffer window marcopolo-mode--repository-buffer))
   ;; No window, but the buffer is already being displayed somewhere.
   ((get-buffer-window marcopolo-mode--repository-buffer 'visible))
   ;; Neither, so we create the window.
   (t (pop-to-buffer marcopolo-mode--repository-buffer)))
  marcopolo-mode--repository-buffer)


(defun marcopolo-mode--display-repository (repository &optional window)
  "Display informations given by `REPOSITORY' on `WINDOW'.
If `WINDOW' is nil, use selected one.

Returns the repository buffer."
  (with-current-buffer
      (marcopolo-mode--display-repository-buffer window)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (marcopolo-mode)
      (marcopolo--render-repository-informations repository)
      (current-buffer))))


(defun marcopolo--render-image (image)
  "Render a `IMAGE' to the Marcopolo buffer."
  (let ((image-data (s-split ":" (elt (plist-get image :RepoTags) 0))))
    (widget-insert
     (format "%30s %10s %15s %10s\n"
             (propertize (first image-data)
                         ;; (plist-get image :Id)
                         'face 'marcopolo-image-name)
             (propertize (second image-data)
                         ;; (format "%s" (plist-get image :RepoTags))
                         'face 'marcopolo-repository-misc)
             (propertize (format "%d" (plist-get image :VirtualSize))
                         'face 'marcopolo-repository-description)
             (propertize (format "%d" (plist-get image :Created))
                         'face 'marcopolo-repository-misc)))))


(defun marcopolo--render-images (images)
  "Render `IMAGES'."
  (let ((start (point)))
    (widget-insert
     (format "%30s %10s %15s %10s\n\n"
             "Name" "Tag" "ID" "Created"))
    (cl-loop
     for n from 1 to (length images)
     do (let ((image (elt images (- n 1)))
              (start (point)))
          (marcopolo--render-image image)
          (put-text-property start (point) :marcopolo-image image)))
    (widget-insert "\n")))


(defun marcopolo--render-container (container)
  "Render a `CONTAINER' to the Marcopolo buffer."
  (widget-insert
   (format "%12s %30s %20s %10s\n"
           (propertize (s-left 12 (plist-get container :Id))
                       'face 'marcopolo-image-name)
           (propertize (plist-get container :Image)
                       'face 'marcopolo-repository-misc)
           (propertize (plist-get container :Status)
                       'face 'marcopolo-repository-description)
           (propertize (format "%d" (plist-get container :Created))
                       'face 'marcopolo-repository-description)
           )))


(defun marcopolo--render-containers (containers)
  "Render `CONTAINERS'."
  (let ((start (point)))
    (widget-insert
     (format "%12s %30s %20s %10s\n\n"
             "Container ID" "Image" "Status" "Created"))
    (cl-loop
     for n from 1 to (length containers)
     do (let ((container (elt containers (- n 1)))
              (start (point)))
          (marcopolo--render-container container)
          (put-text-property start (point) :marcopolo-container container)))
    (widget-insert "\n")))


;; Mode


(defun marcopolo-kill-buffer ()
  "Kill the `marcopolo-buffer' and delete the current window."
  (interactive)
  (let ((buffer (current-buffer)))
    (when (equal buffer (current-buffer))
      (unless (one-window-p)
        (delete-window)))
    (when buffer
      (kill-buffer buffer))))


(defun marcopolo-describe-repository ()
  "Display informations about the Docker repository at point."
  (interactive)
  (let ((repository (marcopolo-current-repository)))
    (when repository
      (let ((window (get-buffer-window
                     (marcopolo-mode--display-repository repository))))
        (switch-to-buffer marcopolo-mode--repository-buffer)))))


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


(defmacro marcopolo--with-widget (title site &rest body)
  `(progn
     (set-buffer (get-buffer-create marcopolo-buffer))
     (switch-to-buffer-other-window marcopolo-buffer)
     (kill-all-local-variables)
     (setq marcopolo-mode--docker-site ,site)
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
    (define-key map (kbd "d") 'marcopolo-describe-repository)
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
(defun marcopolo-mode-registry-search (term)
  "Search from Docker registry repositories using `TERM' request."
  (interactive
   (list (read-from-minibuffer "Search: "
                               (car marcopolo-mode-history)
                               nil
                               nil
                               'marcopolo-mode-history)))
  (let ((registry (marcopolo-get-registry-client)))
    (marcopolo--with-widget
     (propertize "Docker registry repositories :")
     'registry
     (condition-case err
         (marcopolo--render-repositories
          (marcopolo--assoc-cdr 'results (marcopolo-registry-search registry term)))
       (marcopolo-error
        (message "%s" (error-message-string err)))))))


;;;###autoload
(defun marcopolo-mode-hub-search (term)
  "Search from Docker Hub repositories using `TERM' request."
  (interactive
   (list (read-from-minibuffer "Search: "
                               (car marcopolo-mode-history)
                               nil
                               nil
                               'marcopolo-mode-history)))
  (let ((hub (marcopolo-get-hub-client)))
    (marcopolo--with-widget
     (propertize "Docker HUB repositories :")
     'hub
     (condition-case err
         (marcopolo--render-repositories
          (marcopolo--assoc-cdr 'results (marcopolo-hub-search hub term)))
       (marcopolo-error
        (message "%s" (error-message-string err)))))))



;;;###autoload
(defun marcopolo-docker-images ()
  (interactive)
  (let ((client (marcopolo-get-api-client)))
    (marcopolo--with-widget
     (propertize "Docker images :")
     'client
     (condition-case err
         (marcopolo--render-images
          (marcopolo--list-images client nil))
       (marcopolo-error
        (message "%s" (error-message-string err)))))))


;;;###autoload
(defun marcopolo-docker-containers ()
  (interactive)
  (let ((client (marcopolo-get-api-client)))
    (marcopolo--with-widget
     (propertize "Docker images :")
     'client
     (condition-case err
         (marcopolo--render-containers
          (marcopolo--list-containers client nil))
       (marcopolo-error
        (message "%s" (error-message-string err)))))))



(provide 'marcopolo-mode)
;;; marcopolo-mode.el ends here
