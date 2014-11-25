;;; marcopolo-registry.el --- Docker registry client

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

(require 'marcopolo-utils)


(defun marcopolo--registry-status ()
  "Status check for registry."
  (marcopolo--perform-http-request "GET" "_ping" nil 200))


(defun marcopolo--registry-search (term)
  "Search the Index given a search `TERM'."
;;  (marcopolo--perform-http-request "GET" "search" (list (cons "q" term)) 200))
  (marcopolo--perform-http-request "GET"
                                   (s-concat "search?q=" term)
                                   nil
                                   200))

(defun marcopolo--registry-repositories-tags (namespace repository)
  "Get all of the tags for the given repository.
`NAMESPACE' is the namespace for the repository
`REPOSITORY' is the name for the repository"
  (let ((uri (s-concat "repositories/" namespace "/" repository "/tags")))
    (marcopolo--perform-http-request "GET" uri nil 200)))

(defun marcopolo--registry-repository-tag-imageid (namespace repository tag)
  "Get a tag for the given repository.
`NAMESPACE' is the namespace for the repository
`REPOSITORY' is the name for the repository
`TAG' is the name of tag you want to get"
  (let ((uri (s-concat "repositories/" namespace "/" repository "/tags/" tag)))
    (marcopolo--perform-http-request "GET" uri nil 200)))


(provide 'marcopolo-registry)
;;; marcopolo-registry.el ends here
