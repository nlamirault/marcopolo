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



;; Network tools

(defvar marcopolo--api-process-data nil)

(defvar marcopolo--api-process-finish nil)

(defconst marcopolo--api-process-name "marcopolo-api")

(defconst marcopolo--api-process-buffer "*marcopolo-api*")


(defun marcopolo--api-process-filter (process msg)
  (setq marcopolo--api-process-data
        (concat marcopolo--api-process-data msg)))


(defun marcopolo--api-process-sentinel (process msg)
  (when (memq (process-status process) '(closed exit signal))
    (setq marcopolo--api-process-finish t)))


(defun marcopolo--get-remote-api-process ()
  "Retrieve or create the process to communicate with the Docker daemon."
  (or (get-buffer-process marcopolo--api-process-buffer)
      (make-network-process :name marcopolo--api-process-name
                            :buffer marcopolo--api-process-buffer
                            :family 'local
                            :host nil
                            :service "/var/run/docker.sock"
                            :sentinel 'marcopolo--api-process-sentinel
                            :filter 'marcopolo--api-process-filter)))


(defun marcopolo--api-process-request (request)
  "Send to the Docker daemon the API `REQUEST'."
  (setq marcopolo--api-process-data nil)
  (setq marcopolo--api-process-finish nil)
  (let* ((docker-process (marcopolo--get-remote-api-process)))
    (when marcopolo-debug
      (message "[MarcoPolo] Process Request: %s" request))
    (process-send-string docker-process request)
    (while (not marcopolo--api-process-finish)
      (accept-process-output docker-process 5))
    marcopolo--api-process-data))


(defun marcopolo--api-request (method path &optional params)
  "Send to Docker remote API a request.
`METHOD' is the HTTP method
`PATH': is the requested URI
`PARAMS' is a CONS list of HTTP parameters"
  (when marcopolo-debug
    (message "[MarcoPolo] API Request: %s %s %s" method path params))
  (let* ((uri (if params
                  (s-concat path "?" (marcopolo--list-to-http-parameters params))
                path))
         (request (format "%s %s HTTP/1.0\r\n\r\n" method uri))
         (json-object-type 'plist)
         (output (marcopolo--api-process-request request)))
    (when marcopolo-debug
      (message "[MarcoPolo] API Response: %s" output))
    (if (s-contains? "application/json" output)
        (progn
          (message "JSON  content")
          (marcopolo--docker-api-response-to-plist output))
      (marcopolo--docker-api-response-to-text output))))


;; Assoc tools


(defun marcopolo--assoc-cdr (key list)
  "Return value of `KEY' into `LIST' otherwise an empty string."
  (let ((result (cdr (assoc key list))))
    (if result
        result
      "")))


(defun marcopolo--docker-api-response-to-plist (string)
  "Return a `PLIST' from `STRING' received from the Docker daemon."
  (let ((index (s-index-of "\r\n\r\n" string))
        (json-object-type 'plist))
    (json-read-from-string (substring string (+ 4 index)))))

(defun marcopolo--docker-api-response-to-text (string)
  "Return the response text from `STRING' received from the Docker daemon."
  (let ((index (s-index-of "\r\n\r\n" string)))
    (substring string (+ 4 index))))


(defun marcopolo--list-to-http-parameters (params)
  "Transform `PARAMS' to parameters KEY=VALUE for HTTP request."
  (s-join "&" (mapcar #'(lambda (param)
                         (format "%s=%s" (car param) (cdr param)))
                      params)))


(provide 'marcopolo-utils)
;;; marcopolo-utils.el ends here
