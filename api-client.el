;;; api-client --- Emacs web API client
;;;---------------------------------------------------------
;;; Commentary:
;;  Test Cowboy API server.
;;  Usage:
;;  - Select region which is an Erlang shell command.
;;  - Call execution and value is returned in new buffer
;;    *api-client/buffer*.

;;;---------------------------------------------------------
;;; Code:

(defvar api-client-dependencies '(request)
  "Packages to be installed as dependencies.")

(require 'package)

(mapc #'(lambda (package)
          "Install dependencies."
          (unless (package-installed-p package)
            (package-install package)))
      api-client-dependencies)

(require 'request)
(require 'json)

(defun api-client-call ()
  "Call API and put answer into *api-client/buffer*."
  (interactive)
  (let ((body (buffer-substring-no-properties
               (region-beginning)
               (region-end))))
    (request
     "localhost:8080/api"
     :type "POST"
     :data (json-encode `(("cmd" . ,body)))
     :parser 'json-read
     :success (cl-function
               (lambda (&key data &allow-other-keys)
                 (let ((answer (assoc-default 'ans data)))
                   (with-output-to-temp-buffer
                       "*api-client/buffer*"
                     (princ answer))))))))
(global-set-key (kbd "C-c a p i") 'api-client-call)

(provide 'api-client)
;;; api-client.el ends here
