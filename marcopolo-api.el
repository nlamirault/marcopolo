;;; marcopolo-api.el --- Marcopolo API settings.

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


(require 'marcopolo-custom)


(defconst marcopolo--user-agent "marcopolo"
  "The user agent for Marcopolo.")

(defconst marcopolo--docker-api-version "v1"
  "The Docker API version.")

(defconst marcopolo--hub-username-key "DOCKER_HUB_USERNAME"
  "Environment variable name for DOCKER_HUB_USERNAME.")

(defconst marcopolo--hub-password-key "DOCKER_HUB_PASSWORD"
  "Environment variable name for DOCKER_HUB_USERNAME.")

(defconst marcopolo--registry-host-key "DOCKER_REGISTRY_HOST"
  "Environment variable name for DOCKER_REGISTRY_HOST.")

(defconst marcopolo--registry-username-key "DOCKER_REGISTRY_USERNAME"
  "Environment variable name for DOCKER_REGISTRY_USERNAME.")

(defconst marcopolo--registry-password-key "DOCKER_REGISTRY_PASSWORD"
  "Environment variable name for DOCKER_REGISTRY_USERNAME.")

(defun marcopolo--get-registry-host ()
  "Retrieve the Docker registry host.
Use `marcopolo-registry-host' or DOCKER_REGISTRY_HOST environment variable"
  (if marcopolo-registry-host
      marcopolo-registry-host
    (getenv marcopolo--registry-host-key)))

(defun marcopolo--get-registry-username ()
  "Retrieve the Docker Registry username.
Use `marcopolo-registry-username' or DOCKER_REGISTRY_USERNAME environment variable"
  (if marcopolo-registry-username
      marcopolo-registry-username
    (getenv marcopolo--registry-username-key)))

(defun marcopolo--get-registry-password ()
  "Retrieve the Docker Registry password.
Use `marcopolo-registry-password' or DOCKER_REGISTRY_PASSWORD environment variable"
  (if marcopolo-registry-password
      marcopolo-registry-password
    (getenv marcopolo--registry-password-key)))


(defun marcopolo--get-hub-username ()
  "Retrieve the Docker Hub username.
Use `marcopolo-hub-username' or DOCKER_HUB_USERNAME environment variable"
  (if marcopolo-hub-username
      marcopolo-hub-username
    (getenv marcopolo--hub-username-key)))

(defun marcopolo--get-hub-password ()
  "Retrieve the Docker Hub password.
Use `marcopolo-hub-password' or DOCKER_HUB_PASSWORD environment variable"
  (if marcopolo-hub-password
      marcopolo-hub-password
    (getenv marcopolo--hub-password-key)))


(provide 'marcopolo-api)
;;; marcopolo-api.el ends here
