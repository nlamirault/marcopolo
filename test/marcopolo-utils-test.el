;;; marcopolo-utils-test.el --- Tests for utils package

;; Copyright (C) 2015 Nicolas Lamirault <nicolas.lamirault@gmail.com>

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either utils 2
;; of the License, or (at your option) any later utils.

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


(ert-deftest test-marcopolo-api-text-to-string ()
  :tags '(utils current)
  (with-test-sandbox
   (let* ((output "HTTP/1.0 200 OK
          (text (marcopolo--docker-api-response-to-text output)))
     (should (string-equal "OK\n" text)))))


(ert-deftest test-marcopolo-api-json-to-plist ()
  :tags '(utils)
  (with-test-sandbox
   (let* ((output "HTTP/1.0 200 OK
          (json (marcopolo--docker-api-response-to-plist output)))
     (should (string-equal "1.6.0" (plist-get json :Version)))
     (should (string-equal "linux" (plist-get json :Os)))
     (should (string-equal "3.19.3-3-ARCH" (plist-get json :KernelVersion)))
     (should (string-equal "go1.4.2" (plist-get json :GoVersion)))
     (should (string-equal "4749651" (plist-get json :GitCommit)))
     (should (string-equal "amd64" (plist-get json :Arch)))
     (should (string-equal "1.18" (plist-get json :ApiVersion))))))


(ert-deftest test-marcopolo-list-to-http-parameters ()
  :tags '(utils)
  (with-test-sandbox
   (let* ((params '((size . 3)
                    ("foo" . "bar")
                    ("hello" . "world")))
          (request (marcopolo--list-to-http-parameters params)))
     (should (string-equal "size=3&foo=bar&hello=world" request)))))



(provide 'marcopolo-utils-test)
;;; marcopolo-utils-test.el ends here