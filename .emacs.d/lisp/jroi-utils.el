;;; Shell

(defun jroi-shell ()
  "Call `shell', but always prompt for buffer."
  (interactive)
  (let ((current-prefix-arg '(4)))
    (call-interactively #'shell)))

;;; Select sexp

(defvar-local jroi-select-sexp-prev-point nil
  "Point before calling `jroi-select-sexp-point'.")

(defun jroi-select-sexp ()
  "Select the region between the delimiters of the sexp at
point."
  (interactive)
  (let* ((start (save-excursion
		  (backward-up-list nil t t)
		  (point)))
	 (end (save-excursion
		(goto-char start)
		(forward-sexp)
		(point))))
    (setq jroi-select-sexp-prev-point (point))
    (goto-char (+ start 1))
    (push-mark (- end 1) nil t)))

(defun jroi-select-sexp-restore-point ()
  "Restore point before calling `jroi-select-sexp`."
  (when (and (eq last-command 'jroi-select-sexp)
	     jroi-select-sexp-prev-point)
    (goto-char jroi-select-sexp-prev-point)))

(advice-add 'keyboard-quit :before #'jroi-select-sexp-restore-point)

;;; Kill other buffers

(defcustom jroi-kill-other-buffers-keep-regexps '("\\` .*" ; internal buffers
						  "\\`\\*Messages\\*\\'"
						  "\\`\\*scratch\\*\\'")
  "List of regexp saying which buffers will not be killed by
`jroi-kill-other-buffers'."
  :group 'jroi
  :type '(repeat (regexp :tag "Regexp matching Buffer Name")))

(defun jroi-kill-other-buffers ()
  "Kill all other buffers, unless they match any regexp in
`jroi-kill-other-buffers-keep-regexps'."
  (interactive)
  (mapc #'(lambda (buf)
	    (let ((bufname (buffer-name buf)))
	      (unless (cl-find bufname jroi-kill-other-buffers-keep-regexps
			       :test #'(lambda (bn re) (string-match re bn)))
		(kill-buffer buf))))
	(delq (current-buffer) (buffer-list))))
