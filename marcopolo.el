;;; marcopolo.el --- Emacs client for Docker API

;; Author: Nicolas Lamirault <nicolas.lamirault@gmail.com>
;; URL: https://github.com/nlamirault/marcopolo
;; Version: 0.1.0
;; Keywords: docker

;; Package-Requires: ((s "1.9.0") (dash "2.9.0") (pkg-info "0.5.0") (request "0.1.0"))

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

;; Provides a Docker API client for Emacs.

;;; Installation:

;; Available as a package in melpa.milkbox.net.

;; (add-to-list 'package-archives
;;              '("melpa" . "http://melpa.milkbox.net/packages/") t)
;;
;; M-x package-install marcopolo

;;; Usage:



;;; Code:

(require 's)

;; Customization

(defgroup marcopolo nil
  "Docker API client for Emacs."
  :group 'applications
  :link '(url-link :tag "Github" "https://github.com/nlamirault/marcopolo")
  :link '(emacs-commentary-link :tag "Commentary" "marcopolo"))

;; MarcoPolo library

(require 'marcopolo-version)
(require 'marcopolo-utils)
(require 'marcopolo-registry)


;;;###autoload
(defun marcopolo-version (&optional show-version)
  "Get the marcopolo version as string.
If called interactively or if SHOW-VERSION is non-nil, show the
version in the echo area and the messages buffer.
The returned string includes both, the version from package.el
and the library version, if both a present and different.
If the version number could not be determined, signal an error,
if called interactively, or if SHOW-VERSION is non-nil, otherwise
just return nil."
  (interactive (list (not (or executing-kbd-macro noninteractive))))
  (let* ((version (marcopolo--library-version)))
    (unless version
      (error "Could not find out marcopolo version"))
    (message "marcopolo %s" version)
    version))


(provide 'marcopolo)
;;; marcopolo.el ends here
