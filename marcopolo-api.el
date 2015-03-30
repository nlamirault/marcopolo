;;; marcopolo-api.el --- Marcopolo API settings.

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


(require 'marcopolo-utils)


(defun marcopolo-search (term site)
  "Search `TERM' on `SITE' (which is 'registry or 'hub)."
  (let ((uri (s-concat "search?q=" term)))
    (marcopolo--request "GET" uri nil 200 site)))

(defun marcopolo-repository-tags (namespace repository site)
  "Get all of the tags for the given repository.
`NAMESPACE' is the namespace for the repository
`REPOSITORY' is the name for the repository
`SITE' could be 'registry or 'hub"
  (let ((uri (s-concat "repositories/" namespace "/" repository "/tags")))
    (marcopolo--request "GET" uri nil 200 site)))

(defun marcopolo-repository-images (namespace repository site)
  "Get the images for a user repository.
`NAMESPACE' is the namespace for the repository
`REPOSITORY' is the name for the repository
`SITE' could be 'registry or 'hub"
  (let ((uri (s-concat "repositories/" namespace "/" repository "/images")))
    (marcopolo--request "GET" uri nil 200 site)))

(defun marcopolo-repository-tag-imageid (namespace repository tag site)
  "Get a tag for the given repository.
`NAMESPACE' is the namespace for the repository
`REPOSITORY' is the name for the repository
`TAG' is the name of tag you want to get
`SITE' could be 'registry or 'hub"
  (let ((uri (s-concat "repositories/" namespace "/" repository "/tags/" tag)))
    (marcopolo--request "GET" uri nil 200 site)))

(defun marcopolo-image-layer (image-id site)
  "Get image layer.
`IMAGE-ID' – the id for the layer you want to get
`SITE' could be 'registry or 'hub"
  (let ((uri (s-concat "images/" image-id "/json")))
    (marcopolo--request "GET" uri nil 200 site)))

(defun marcopolo-hub-login ()
  "Try you login."
  (marcopolo--request "GET" "users" nil 200 'hub))

(defun marcopolo-registry-status ()
  "Status check for registry."
  (marcopolo--request "GET" "_ping" nil 200 'registry))

(provide 'marcopolo-api)
;;; marcopolo-api.el ends here
