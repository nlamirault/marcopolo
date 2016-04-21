;;; marcopolo-hub.el --- Docker Hub client

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

(require 'marcopolo-utils)


(defun marcopolo--hub-login ()
  "Try you login."
  (marcopolo--with-request
   (marcopolo--perform-hub-request "GET" "users" nil 200)))


(defun marcopolo--hub-search (term)
  "Search the Docker Hub given a search `TERM'."
  (marcopolo--with-request
   (let ((uri (s-concat "search?q=" term)))
     (marcopolo--perform-hub-request "GET" uri nil 200))))


(defun marcopolo--hub-repository-images (namespace repository)
  "Get the images for a user repository.
`NAMESPACE' is the namespace for the repository
`REPOSITORY' is the name for the repository"
  (marcopolo--with-request
   (let ((uri (s-concat "repositories/" namespace "/" repository "/images")))
     (marcopolo--perform-hub-request "GET" uri nil 200))))


;; (defun marcopolo--hub-create-repository (namespace repository)
;;   "Create a user repository.
;; `NAMESPACE' is the namespace for the repository
;; `REPOSITORY' is the name for the repository"
;;   (let ((uri (s-concat "repositories/" namespace "/" repository)))
;;     (marcopolo--perform-hub-request "PUT"
;;                                     uri
;;                                     nil 200)))


(provide 'marcopolo-hub)
;;; marcopolo-hub.el ends here
