;; test-helper.el --- Test helpers for marcopolo.el

;; Copyright (C) Nicolas Lamirault <nicolas.lamirault@gmail.com>

;; Author: Nicolas Lamirault <nicolas.lamirault@gmail.com>
;; Homepage: https://github.com/nlamirault/marcopolo

;;; License:

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'ansi)
(require 'cl) ;; http://emacs.stackexchange.com/questions/2864/symbols-function-definition-is-void-cl-macroexpand-all-when-trying-to-instal
(require 'f)
(require 'undercover)

(setq debugger-batch-max-lines (+ 50 max-lisp-eval-depth)
      debug-on-error t)

(setq marcopolo-debug nil)

(setenv "DOCKER_REGISTRY_HOST" "https://registry.hub.docker.com")

(defvar username (getenv "HOME"))

(defconst marcopolo-testsuite-dir
  (f-parent (f-this-file))
  "The testsuite directory.")

(defconst marcopolo-source-dir
  (f-parent marcopolo-testsuite-dir)
  "The marcopolo.el source directory.")

(defconst marcopolo-sandbox-path
  (f-expand "sandbox" marcopolo-testsuite-dir)
  "The sandbox path for marcopolo.")

(defun cleanup-load-path ()
  "Remove home directory from 'load-path."
  (message (ansi-green "[marcopolo] Cleanup path"))
  (mapc #'(lambda (path)
            (when (string-match (s-concat username "/.emacs.d") path)
              (message (ansi-yellow "Suppression path %s" path))
              (setq load-path (delete path load-path))))
        load-path))

(defun load-unit-tests (path)
  "Load all unit test from PATH."
  (message (ansi-green "[marcopolo] Execute unit tests %s"
                       path))
  (dolist (test-file (or argv (directory-files path t "-test.el$")))
    (load test-file nil t)))


(defun load-library (file)
  "Load current library from FILE."
  (let ((path (s-concat marcopolo-source-dir file)))
    (message (ansi-yellow "[marcopolo] Load library from %s" path))
    (undercover "*.el" (:exclude "*-test.el"))
    (require 'marcopolo))); path)))


(defun setup-marcopolo ()
  "Setup Marcopolo token from MARCOPOLO_TOKEN environment variable."
  (setq marcopolo-token-id (getenv "MARCOPOLO_TOKEN")))


(provide 'test-helper)
;;; test-helper.el ends here
