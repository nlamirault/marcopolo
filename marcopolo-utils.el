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

(defun marcopolo--get-rest-uri (uri site)
  "Generate Docker REST request.
`URI' is the REST request
`SITE' could be 'registry or 'hub"
  (let ((host (if (eql 'registry site)
                  (marcopolo--get-registry-host)
                marcopolo-hub-host)))
    (if host
        (s-concat host "/" marcopolo--docker-api-version "/" uri)
      (error (signal 'marcopolo-error '("Docker host unknown."))))))


(defun marcopolo--create-basic-auth (username password)
  "Create the BASIC AUTH value from `USERNAME' and `PASSWORD'."
  (base64-encode-string (s-concat username ":" password)))

;; (defun marcopolo--get-headers (site) ; &optional auth)
;;   "Generate HTTP headers for Docker HUB or Registry.
;; `SITE' could be 'registry or 'hub"
;;   (let ((headers
;;          (list (cons "Accept" "application/json")
;;                (cons "Content-Type" "application/json")
;;                (cons "User-Agent"
;;                      (s-concat marcopolo--user-agent
;;                                "/"
;;                                (marcopolo--library-version))))))
;;     (if (eql 'registry site)
;;         ;;(if  auth
;;         (let ((username (marcopolo--get-registry-username))
;;               (password (marcopolo--get-registry-password)))
;;           (when (and username password)
;;             (let ((auth (marcopolo--create-basic-auth username password)))
;;               (cons (cons "Authorization" (concat "Basic " auth))
;;                     headers))))
;;       (cons (cons "Authorization"
;;                   (concat "Basic "
;;                           (marcopolo--create-basic-auth
;;                            (marcopolo--get-hub-username)
;;                            (marcopolo--get-hub-password))))
;;             headers))))

(defun marcopolo--get-headers (site) ; &optional auth)
  "Generate HTTP headers for Docker HUB or Registry.
`SITE' could be 'registry or 'hub"
  (let* ((username (if (eql 'registry site)
                       (marcopolo--get-registry-username)
                     (marcopolo--get-hub-username)))
         (password (if (eql 'registry site)
                       (marcopolo--get-registry-password)
                     (marcopolo--get-hub-password)))
         (headers
          (list (cons "Accept" "application/json")
                (cons "Content-Type" "application/json")
                (cons "User-Agent"
                      (s-concat marcopolo--user-agent
                                "/"
                                (marcopolo--library-version)))
                (cons "Authorization"
                      (concat "Basic "
                              (marcopolo--create-basic-auth username password))))))
    headers))

(defun marcopolo--perform-http-request (method uri headers params status-code)
  "Do a HTTP METHOD request using URI, HEADERS and PARAMS.
If HTTP return code is STATUS-CODE, send the response content otherwise
raise an error."
  (when marcopolo-debug
    (message "[MarcoPolo] HTTP Request: %s %s %s" uri headers params))
  (let ((response (request uri ;(marcopolo--get-rest-uri uri)
                           :type method
                           :headers headers ;(marcopolo--get-headers)
                           :sync t
                           :data params
                           :parser 'json-read)))
    (if (= status-code (request-response-status-code response))
        (request-response-data response)
      (error
       (signal 'marcopolo-http-error
               (list (request-response-status-code response)
                     (request-response-error-thrown response)))))))

(defun marcopolo--request (method path params status-code site)
  "Do a Docker API request.
`METHOD' is the HTTPO method
`PATH' is the URI of the request
`PARAMS' is a cons of HTTP parameters
`STATUS-CODE' is the HTTP return desired code
`SITE' could be 'registry or 'hub"
  (marcopolo--perform-http-request method
                                   (marcopolo--get-rest-uri path site)
                                   (marcopolo--get-headers site)
                                   params
                                   status-code))

(defun marcopolo--get-registry-host ()
  "Retrieve the Docker registry host.
Use `marcopolo-registry-host' or DOCKER_REGISTRY_HOST environment variable"
  (if marcopolo-registry-host
      marcopolo-registry-host
    (getenv marcopolo--registry-host-key)))

(defun marcopolo--get-registry-username ()
  "Retrieve the Docker Registry username.
Use `marcopolo-registry-username' or DOCKER_REGISTRY_USERNAME environment variable"
  (if marcopolo-registry-username
      marcopolo-registry-username
    (getenv marcopolo--registry-username-key)))

(defun marcopolo--get-registry-password ()
  "Retrieve the Docker Registry password.
Use `marcopolo-registry-password' or DOCKER_REGISTRY_PASSWORD environment variable"
  (if marcopolo-registry-password
      marcopolo-registry-password
    (getenv marcopolo--registry-password-key)))


(defun marcopolo--get-hub-username ()
  "Retrieve the Docker Hub username.
Use `marcopolo-hub-username' or DOCKER_HUB_USERNAME environment variable"
  (if marcopolo-hub-username
      marcopolo-hub-username
    (getenv marcopolo--hub-username-key)))

(defun marcopolo--get-hub-password ()
  "Retrieve the Docker Hub password.
Use `marcopolo-hub-password' or DOCKER_HUB_PASSWORD environment variable"
  (if marcopolo-hub-password
      marcopolo-hub-password
    (getenv marcopolo--hub-password-key)))


;; Assoc tools

(defun marcopolo--assoc-cdr (key list)
  (let ((result (cdr (assoc key list))))
    (if result
        result
      "")))


(provide 'marcopolo-utils)
;;; marcopolo-utils.el ends here
