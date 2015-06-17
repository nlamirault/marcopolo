;;; marcopolo-utils.el --- some tools

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

(require 'json)
(require 'request)
(require 's)


(require 'marcopolo-custom)
(require 'marcopolo-commons)
(require 'marcopolo-version)


;; Errors

(eval-and-compile
  (unless (fboundp 'define-error)
    ;; Shamelessly copied from Emacs trunk :)
    (defun define-error (name message &optional parent)
      "Define NAME as a new error signal.
MESSAGE is a string that will be output to the echo area if such an error
is signaled without being caught by a `condition-case'.
PARENT is either a signal or a list of signals from which it inherits.
Defaults to `error'."
      (unless parent (setq parent 'error))
      (let ((conditions
             (if (consp parent)
                 (apply #'nconc
                        (mapcar (lambda (parent)
                                  (cons parent
                                        (or (get parent 'error-conditions)
                                            (error "Unknown signal `%s'" parent))))
                                parent))
               (cons parent (get parent 'error-conditions)))))
        (put name 'error-conditions
             (delete-dups (copy-sequence (cons name conditions))))
        (when message (put name 'error-message message))))))

(define-error 'marcopolo-error "Marcopolo error")

(define-error 'marcopolo-http-error "HTTP Error" 'marcopolo-error)


;; HTTP tools


(defun marcopolo--perform-http-request (method uri headers params status-code)
  "Do a Docker API request.
`METHOD' is the HTTP method
`URI' is the URI of the HTTP request
`HEADERS' is the HTTP request headers
`PARAMS' is a cons of HTTP parameters
`STATUS-CODE' is the HTTP return desired code
If HTTP return code is STATUS-CODE, send the response content otherwise
raise an error."
  (when marcopolo-debug
    (message "[MarcoPolo] HTTP Request: %s %s %s" uri headers params))
  (let ((response (request uri
                           :type method
                           :headers headers
                           :sync t
                           :data params
                           :parser 'json-read)))
    (if (= status-code (request-response-status-code response))
        (let ((data (request-response-data response)))
          (when marcopolo-debug
            (message "[MarcoPolo] HTTP Response: %s" data))
          data)
      (error
       (signal 'marcopolo-http-error
               (list (request-response-status-code response)
                     (request-response-error-thrown response)))))))


;; Assoc tools

(defun marcopolo--assoc-cdr (key list)
  "Return value of `KEY' into `LIST' otherwise an empty string."
  (let ((result (cdr (assoc key list))))
    (if result
        result
      "")))


(provide 'marcopolo-utils)
;;; marcopolo-utils.el ends here
