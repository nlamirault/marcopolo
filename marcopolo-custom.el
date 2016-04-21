;;; marcopolo-custom.el --- Customization group of Marcopolo

;; Copyright (C) 2015, 2016 Nicolas Lamirault <nicolas.lamirault@gmail.com>

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

;; Customization

(defgroup marcopolo nil
  "Docker API client for Emacs."
  :group 'applications
  :link '(url-link :tag "Github" "https://github.com/nlamirault/marcopolo")
  :link '(emacs-commentary-link :tag "Commentary" "marcopolo"))


(defcustom marcopolo-hub-host "https://index.docker.io"
  "The Docker Hub API endpoint."
  :type 'string
  :group 'marcopolo)

(defcustom marcopolo-hub-username nil
  "Username for the Docker Hub."
  :type 'string
  :group 'marcopolo)

(defcustom marcopolo-hub-password nil
  "Password for the Docker Hub."
  :type 'string
  :group 'marcopolo)

(defcustom marcopolo-registry-host nil
  "The Docker registry API endpoint."
  :type 'string
  :group 'marcopolo)

(defcustom marcopolo-registry-username nil
  "Username for the Docker Registry."
  :type 'string
  :group 'marcopolo)

(defcustom marcopolo-registry-password nil
  "Password for the Docker Registry."
  :type 'string
  :group 'marcopolo)

(defcustom marcopolo-debug t
  "Enable or not some logs."
  :type 'string
  :group 'marcopolo)



(provide 'marcopolo-custom)
;;; marcopolo-custom.el ends here
