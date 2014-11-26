;;; marcopolo-api-test.el --- Tests for API commons

;; Copyright (C) Nicolas Lamirault <nicolas.lamirault@gmail.com>

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

(require 'marcopolo-api)

(defun check-marcopolo-settings-from-environment (var value fct)
  (let ((backup (getenv var)))
    (unwind-protect
        (progn
          (setenv var value)
          (should (string= value (funcall fct))))
      (setenv var backup))))

(ert-deftest test-marcopolo-get-username-from-environment ()
  (check-marcopolo-settings-from-environment
   "DOCKER_HUB_USERNAME" "foo" 'marcopolo--get-hub-username))

(ert-deftest test-marcopolo-get-username-from-conf ()
  (let* ((value "foo")
        (marcopolo--hub-username value))
    (should (string= value (marcopolo--get-hub-username)))))

(ert-deftest test-marcopolo-get-password-from-environment ()
  (check-marcopolo-settings-from-environment
   "DOCKER_HUB_PASSWORD" "bar" 'marcopolo--get-hub-password))

(ert-deftest test-marcopolo-get-password-from-conf ()
  (let* ((value "bar")
        (marcopolo--hub-password value))
    (should (string= value (marcopolo--get-hub-password)))))

(ert-deftest test-marcopolo-get-registry-host-from-environment ()
  (check-marcopolo-settings-from-environment
   "DOCKER_REGISTRY_HOST" "http://localhost:8989" 'marcopolo--get-registry-host))

(ert-deftest test-marcopolo-get-registry-host-from-conf ()
  (let* ((value "http://localhost:7878")
        (marcopolo--registry-host value))
    (should (string= value (marcopolo--get-registry-host)))))

(provide 'marcopolo-api-test)
;;; marcopolo-api-test.el ends here
