;;; marcopolo-registry-test.el --- Tests for Docker registry client

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

(ert-deftest test-marcopolo-registry-status ()
  :tags '(registry current)
  (with-test-sandbox
   (let ((response (marcopolo--registry-status)))
     (should (eql t response)))))


(ert-deftest test-marcopolo-registry-search-ubuntu ()
  :tags '(registry)
  (with-test-sandbox
   (let ((response (marcopolo--registry-search "emacs")))
     ;;(message "Response: %s" response)
     (mapc (lambda (result)
             ;;(message "Result: %s" result)
             (should (not (s-blank? (assoc-default 'name result)))))
           (assoc-default 'results response))
     (should (< 0 (assoc-default 'num_results response)))
     (should (string= "emacs" (assoc-default 'query response))))))


(ert-deftest test-marcopolo-registry-repository-tags ()
  :tags '(registry)
  (with-test-sandbox
   (let ((response
          (marcopolo--registry-repositories-tags "nlamirault" "scame")))
     ;;(message "Response: %s" response)
     (should (vectorp response))
     (mapc (lambda (tag)
             ;;(message "Tag: %s" tag)
             (should (not (s-blank? (assoc-default 'name tag))))
             (should (not (s-blank? (assoc-default 'layer tag))))
             (when (string= "0.6.0" (assoc-default 'name tag))
               (should (string= "114b7f0c" (assoc-default 'layer tag))))
             )
           response)
     )))

(ert-deftest test-marcopolo-registry-repository-tag ()
  :tags '(registry)
  (with-test-sandbox
   (let ((response
          (marcopolo--registry-repository-tag-imageid "nlamirault" "scame" "0.6.0")))
     ;; (message "Response: %s" response)
     (should (vectorp response))
     (mapc (lambda (r)
             ;;(message "Tag: %s" tag)
             (should (numberp (assoc-default 'pk r)))
             (should (not (s-blank? (assoc-default 'id r)))))
           response)
     )))

(provide 'marcopolo-registry-test)
;;; marcopolo-registry-test.el ends here
