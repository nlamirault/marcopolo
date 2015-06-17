;;; marcopolo-registry.el --- Marcopolo Registry Client

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

(require 'eieio)


(require 'marcopolo-utils)
(require 'marcopolo-config)

;;
;; Docker Registry
;; -----------------------------------


(defclass marcopolo-docker ()
  ((host :initarg :host
         :type string
         :custom string
         :documentation "The Docker registry hostname")
   (version :initarg :version
            :type string
            :custom string
            :documentation "The Docker registry version implemented")
   (username :initarg :username
             :type string
             :custom string
             :documentation "The registry username used")
   (password :initarg :password
             :type string
             :custom string
             :documentation "The registry password used"))
  "A Docker registry could be : hub, v1 or v2.")


(cl-defmethod marcopolo-docker-get-uri ((registry marcopolo-docker) uri)
  "Generate the HTTP request PATH for the Docker `REGISTRY' using `URI'."
  (with-slots (host version) registry
    (s-concat host "/" version "/" uri)))


(cl-defmethod marcopolo-docker-basic-auth ((registry marcopolo-docker))
  "Generate the basic authentication token for the Docker `REGISTRY'."
  (with-slots (username password) registry
    (base64-encode-string (s-concat username ":" password))))


(cl-defmethod marcopolo-docker-http-headers ((registry marcopolo-docker))
  (list (cons "Accept" "application/json")
        (cons "Content-Type" "application/json")
        (cons "User-Agent"
              (s-concat marcopolo--user-agent
                        "/"
                        (marcopolo--library-version)))
        (cons "Authorization"
              (concat "Basic "
                      (marcopolo-docker-basic-auth registry)))))


(cl-defmethod marcopolo--docker-request ((registry marcopolo-docker) method path params status-code)
  (let ((uri (marcopolo-docker-get-uri registry path))
        (headers (marcopolo-docker-http-headers registry)))
     (marcopolo--perform-http-request method uri headers params status-code)))


(defclass marcopolo-hub (marcopolo-docker)
  ()
  "A Client for the Docker HUB API:
https://docs.docker.com/reference/api/docker-io_api")


(defclass marcopolo-registry-v1 (marcopolo-docker)
  ()
  "A Client for the Docker Registry API v1:
https://docs.docker.com/reference/api/registry_api/ ")


(defclass marcopolo-registry-v2 (marcopolo-docker)
  ()
  "A Client for the Docker Registry API v2:
https://docs.docker.com/registry/overview/")



;; Constructors


(defun marcopolo-get-hub-client ()
  "Return an instant of `marcopolo-hub'."
  (make-instance 'marcopolo-hub
                 :host marcopolo-hub-host
                 :version "v1"
                 :username (marcopolo--get-hub-username)
                 :password (marcopolo--get-hub-password)))

(defun marcopolo-get-registry-client ()
  "Return an instant of `marcopolo-registry'."
  (if (string-equal "v1" (marcopolo--get-registry-version))
      (make-instance 'marcopolo-registry-v1
                     :host (marcopolo--get-registry-host)
                     :username (marcopolo--get-registry-username)
                     :password (marcopolo--get-registry-password)
                     :version "v1")
    (make-instance 'marcopolo-registry-v2
                   :host (marcopolo--get-registry-host)
                   :username (marcopolo--get-registry-username)
                   :password (marcopolo--get-registry-password)
                   :version "v2")))


;; Hub
;; -------------------


(cl-defmethod marcopolo-hub-login ((hub marcopolo-hub))
  (marcopolo--docker-request hub "GET" "users" nil 200))


(cl-defmethod marcopolo-hub-user-repository-images ((hub marcopolo-hub) namespace repository)
  "Get the images for a user repository.
`NAMESPACE' is the namespace for the repository
`REPOSITORY' is the name for the repository"
  (let ((uri (s-concat "repositories/" namespace "/" repository "/images")))
    (marcopolo--docker-request hub "GET" uri nil 200)))


(cl-defmethod marcopolo-hub-search ((registry marcopolo-hub) term)
  "Search `TERM'."
  (let ((uri (s-concat "search?q=" term)))
    (marcopolo--docker-request registry "GET" uri nil 200)))


;; Registry V1
;; -------------------


(cl-defmethod marcopolo-registry-status ((registry marcopolo-registry-v1))
  (marcopolo--docker-request registry "GET" "_ping" nil 200))


(cl-defmethod marcopolo-registry-search ((registry marcopolo-registry-v1) term)
  "Search `TERM'."
  (let ((uri (s-concat "search?q=" term)))
    (marcopolo--docker-request registry "GET" uri nil 200)))


(cl-defmethod marcopolo-registry-repository-tags ((registry marcopolo-registry-v1) namespace repository)
  "Get all of the tags for the given repository.
`NAMESPACE' is the namespace for the repository
`REPOSITORY' is the name for the repository"
  (let ((uri (s-concat "repositories/" namespace "/" repository "/tags")))
    (marcopolo--docker-request registry "GET" uri nil 200)))

;; ;; (defun marcopolo-repository-images (namespace repository site)
;; ;;   "Get the images for a user repository.
;; ;; `NAMESPACE' is the namespace for the repository
;; ;; `REPOSITORY' is the name for the repository
;; ;; `SITE' could be 'registry or 'hub"
;; ;;   (let ((uri (s-concat "repositories/" namespace "/" repository "/images")))
;; ;;     (marcopolo--request "GET" uri nil 200 site)))

(cl-defmethod marcopolo-registry-repository-tag-imageid ((registry marcopolo-registry-v1) namespace repository tag)
  "Get a tag for the given repository.
`NAMESPACE' is the namespace for the repository
`REPOSITORY' is the name for the repository
`TAG' is the name of tag you want to get"
  (let ((uri (s-concat "repositories/" namespace "/" repository "/tags/" tag)))
    (marcopolo--docker-request registry "GET" uri nil 200)))

;; (defun marcopolo-image-layer (image-id site)
;;   "Get image layer.
;; `IMAGE-ID' – the id for the layer you want to get
;; `SITE' could be 'registry or 'hub"
;;   (let ((uri (s-concat "images/" image-id "/json")))
;;     (marcopolo--request "GET" uri nil 200 site)))

(provide 'marcopolo-registry)
;;; marcopolo-registry.el ends here
