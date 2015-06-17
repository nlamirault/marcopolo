;;; marcopolo-commons.el --- Commons for marcopolo

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


(defconst marcopolo--user-agent "marcopolo"
  "The user agent for Marcopolo.")

(defconst marcopolo--docker-api-version "v1.18"
  "The Docker API version.
See: https://docs.docker.com/reference/api/docker_remote_api/")

(defconst marcopolo--hub-username-key "DOCKER_HUB_USERNAME"
  "Environment variable name for DOCKER_HUB_USERNAME.")

(defconst marcopolo--hub-password-key "DOCKER_HUB_PASSWORD"
  "Environment variable name for DOCKER_HUB_PASSWORD.")

(defconst marcopolo--registry-host-key "DOCKER_REGISTRY_HOST"
  "Environment variable name for DOCKER_REGISTRY_HOST.")

(defconst marcopolo--registry-version-key "DOCKER_REGISTRY_VERSION"
  "Environment variable name for DOCKER_REGISTRY_VERSION.")

(defconst marcopolo--registry-username-key "DOCKER_REGISTRY_USERNAME"
  "Environment variable name for DOCKER_REGISTRY_USERNAME.")

(defconst marcopolo--registry-password-key "DOCKER_REGISTRY_PASSWORD"
  "Environment variable name for DOCKER_REGISTRY_PASSWORD.")

(defconst marcopolo--docker-api-username-key "DOCKER_API_USERNAME"
  "Environment variable name for DOCKER_API_USERNAME.")

(defconst marcopolo--docker-api-password-key "DOCKER_API_PASSWORD"
  "Environment variable name for DOCKER_API_PASSWORD.")


(provide 'marcopolo-commons)
;;; marcopolo-commons.el ends here
