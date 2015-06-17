;;; marcopolo-config.el --- Marcopolo configuration tools

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

(require 'marcopolo-custom)
(require 'marcopolo-commons)


(defun marcopolo--get-registry-host ()
  "Retrieve the Docker registry host.
Use `marcopolo-registry-host' or DOCKER_REGISTRY_HOST environment variable"
  (if marcopolo-registry-host
      marcopolo-registry-host
    (getenv marcopolo--registry-host-key)))

(defun marcopolo--get-registry-version ()
  "Retrieve the Docker registry API version.
Use `marcopolo-registry-version' or DOCKER_REGISTRY_VERSION environment variable"
  (if marcopolo-registry-version
      marcopolo-registry-version
    (getenv marcopolo--registry-version-key)))

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



(provide 'marcopolo-config)
;;; marcopolo-config.el ends here
