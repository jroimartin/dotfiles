;;; Rmail utils.

(defun jroi-rmail-url (url)
  "Run Rmail on URL."
  (interactive "sRun rmail on URL: ")
  (let ((filename (make-temp-file "url" nil (url-file-extension url))))
    (url-copy-file url filename 'ok-if-already-exists)
    (rmail-input filename)))
