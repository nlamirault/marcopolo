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


(require 'marcopolo-custom)


(defconst marcopolo--user-agent "marcopolo"
  "The user agent for Marcopolo.")

(defconst marcopolo--docker-api-version "v1"
  "The Docker API version.")


(defun marcopolo--get-registry-host ()
  "Retrieve the Docker registry host.
Use `marcopolo-registry-host' or DOCKER_REGISTRY_HOST environment variable"
  (if marcopolo-registry-host
      marcopolo-registry-host
    (getenv "DOCKER_REGISTRY_HOST")))

(defun marcopolo--get-hub-username ()
  "Retrieve the Docker Hub username.
Use `marcopolo-hub-username' or DOCKER_HUB_USERNAME environment variable"
  (if marcopolo-hub-username
      marcopolo-hub-username
    (getenv "DOCKER_HUB_USERNAME")))

(defun marcopolo--get-hub-password ()
  "Retrieve the Docker Hub password.
Use `marcopolo-hub-password' or DOCKER_HUB_PASSWORD environment variable"
  (if marcopolo-hub-password
      marcopolo-hub-password
    (getenv "DOCKER_HUB_PASSWORD")))


(provide 'marcopolo-api)
;;; marcopolo-api.el ends here
