;;; marcopolo-config-test.el --- Tests for Marcopolo configuration

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


(defun check-marcopolo-settings-from-environment (var value fct)
  (let ((backup (getenv var)))
    (unwind-protect
        (progn
          (setenv var value)
          (should (string= value (funcall fct))))
      (setenv var backup))))


;; Hub

(ert-deftest test-marcopolo-get-hub-username-from-environment ()
  :tags '(config)
  (with-test-sandbox
   (check-marcopolo-settings-from-environment
    marcopolo--hub-username-key "foo" 'marcopolo--get-hub-username)))

(ert-deftest test-marcopolo-get-hub-username-from-conf ()
  :tags '(config)
  (with-test-sandbox
   (let* ((value "foo")
          (marcopolo-hub-username value))
     (should (string= value (marcopolo--get-hub-username))))))

(ert-deftest test-marcopolo-get-hub-password-from-environment ()
  :tags '(config)
  (with-test-sandbox
   (check-marcopolo-settings-from-environment
    marcopolo--hub-password-key "bar" 'marcopolo--get-hub-password)))

(ert-deftest test-marcopolo-get-hub-password-from-conf ()
  :tags '(config)
  (with-test-sandbox
   (let* ((value "bar")
          (marcopolo-hub-password value))
     (should (string= value (marcopolo--get-hub-password))))))


;; Registry

(ert-deftest test-marcopolo-get-registry-username-from-environment ()
  :tags '(config)
  (with-test-sandbox
   (check-marcopolo-settings-from-environment
    marcopolo--registry-username-key "foo" 'marcopolo--get-registry-username)))

(ert-deftest test-marcopolo-get-registry-username-from-conf ()
  :tags '(config)
  (with-test-sandbox
   (let* ((value "foo")
          (marcopolo-registry-username value))
     (should (string= value (marcopolo--get-registry-username))))))

(ert-deftest test-marcopolo-get-registry-password-from-environment ()
  :tags '(config)
  (with-test-sandbox
   (check-marcopolo-settings-from-environment
    marcopolo--registry-password-key "bar" 'marcopolo--get-registry-password)))

(ert-deftest test-marcopolo-get-registry-password-from-conf ()
  :tags '(config)
  (with-test-sandbox
   (let* ((value "bar")
          (marcopolo-registry-password value))
     (should (string= value (marcopolo--get-registry-password))))))

(ert-deftest test-marcopolo-get-registry-host-from-environment ()
  :tags '(config)
  (with-test-sandbox
   (check-marcopolo-settings-from-environment
    marcopolo--registry-host-key "http://localhost:8989" 'marcopolo--get-registry-host)))

(ert-deftest test-marcopolo-get-registry-host-from-conf ()
  :tags '(config)
  (with-test-sandbox
   (let* ((value "http://localhost:7878")
          (marcopolo-registry-host value))
     (should (string= value (marcopolo--get-registry-host))))))

(ert-deftest test-marcopolo-get-registry-version-from-environment ()
  :tags '(config)
  (with-test-sandbox
   (check-marcopolo-settings-from-environment
    marcopolo--registry-version-key "v2" 'marcopolo--get-registry-version)))

(ert-deftest test-marcopolo-get-registry-version-from-conf ()
  :tags '(config)
  (with-test-sandbox
   (let* ((value "v2")
          (marcopolo-registry-version value))
     (should (string= value (marcopolo--get-registry-version))))))


(provide 'marcopolo-config-test)
;;; marcopolo-config-test.el ends here
