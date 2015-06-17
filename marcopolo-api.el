;;; marcopolo-api.el --- Marcopolo remote API Client

;; Copyright (C) 2015 Nicolas Lamirault <nicolas.lamirault@gmail.com>

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

(require 'eieio)


(require 'marcopolo-utils)
(require 'marcopolo-custom)
(require 'marcopolo-config)


(defclass marcopolo-api-client ()
  ((username :initarg :username
             :type string
             :custom string
             :documentation "The remote API username used")
   (password :initarg :password
             :type string
             :custom string
             :documentation "The remote API password used"))
  "The Docker remote API client.
See https://docs.docker.com/reference/api/docker_remote_api/")


(defun marcopolo-get-api-client ()
  "Return an instance of `marcopolo-docker-client'."
  (make-instance 'marcopolo-api-client
                 :username (marcopolo--get-api-username)
                 :password (marcopolo--get-api-password)))


(cl-defmethod marcopolo--docker-api-encode-auth ((client marcopolo-api-client))
  "Encode the auth configuration struct into base64 for the
`X-Registry-Auth' header."
  (with-slots (username password) client
    (base64-encode-string (s-concat username ":" password))))


(cl-defmethod marcopolo--docker-api-http-headers ((client marcopolo-api-client))
  (list (cons "Accept" "application/json")
        (cons "Content-Type" "application/json")
        (cons "User-Agent"
              (s-concat marcopolo--user-agent
                        "/"
                        (marcopolo--library-version)))
        (cons "X-Registry-Auth"
              (marcopolo--docker-api-encode-auth client))))


(cl-defmethod marcopolo--docker-api-version ((client marcopolo-api-client))
  "Show the docker version information."
  (marcopolo--api-request "GET" "/version"))




(provide 'marcopolo-api)
;;; marcopolo-api.el ends here
