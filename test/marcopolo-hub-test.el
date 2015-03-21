;;; marcopolo-hub-test.el --- Tests for Docker Hub client

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


;;(require 'marcopolo)


(ert-deftest test-marcopolo-hub-login ()
  (with-test-sandbox
   (let ((response (marcopolo--hub-login)))
     (should (string= "OK" response)))))

(ert-deftest test-marcopolo-hub-repository-images ()
  (with-test-sandbox
   (let ((response
          (marcopolo--hub-repository-images "nlamirault" "scame")))
     ;;(message "Response: %s" response)
     (should (vectorp response))
     (mapc (lambda (image)
             ;;(message "Image: %s" image)
             (should (not (s-blank? (assoc-default 'id image)))))
           response))))

;; (ert-deftest test-marcopolo-hub-create-repository ()
;;   (let ((response
;;          (marcopolo--hub-create-repository "nlamirault" "foo")))
;;     (message "Response: %s" response)))


(provide 'marcopolo-hub-test)
;;; marcopolo-hub-test.el ends here
