;;; marcopolo-registry-test.el --- Tests for Docker registry client

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


(require 'marcopolo)


(ert-deftest marcopolo-registry-status ()
  (let ((response (marcopolo--registry-status)))
    (should (eql t response))))


(ert-deftest marcopolo-registry-search-ubuntu ()
  (let ((response (marcopolo--registry-search "emacs")))
    ;;(message "Response: %s" response)
    (mapc (lambda (result)
            ;;(message "Result: %s" result)
            (should (not (s-blank? (assoc-default 'name result)))))
          (assoc-default 'results response))
    (should (< 0 (assoc-default 'num_results response)))
    (should (string= "emacs" (assoc-default 'query response)))))


(provide 'marcopolo-registry-test)
;;; marcopolo-registry-test.el ends here
