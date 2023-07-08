;;; Shell

(defun jrm-shell ()
  "Call `shell', but always prompt for buffer."
  (interactive)
  (let ((current-prefix-arg '(4)))
    (call-interactively #'shell)))

;;; Select sexp

(defvar-local jrm-select-sexp-prev-point nil
  "Point before calling `jrm-select-sexp-point'.")

(defun jrm-select-sexp ()
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
    (setq jrm-select-sexp-prev-point (point))
    (goto-char (+ start 1))
    (push-mark (- end 1) nil t)))

(defun jrm-select-sexp-restore-point ()
  "Restore point before calling `jrm-select-sexp`."
  (when (and (eq last-command 'jrm-select-sexp)
	     jrm-select-sexp-prev-point)
    (goto-char jrm-select-sexp-prev-point)))

(advice-add 'keyboard-quit :before #'jrm-select-sexp-restore-point)

;;; Kill other buffers

(defcustom jrm-kill-other-buffers-keep-regexps '("\\` .*" ; internal buffers
						 "\\`\\*Messages\\*\\'"
						 "\\`\\*scratch\\*\\'")
  "List of regexp saying which buffers will not be killed by
`jrm-kill-other-buffers'."
  :group 'jrm
  :type '(repeat (regexp :tag "Regexp matching Buffer Name")))

(defun jrm-kill-other-buffers ()
  "Kill all other buffers, unless they match any regexp in
`jrm-kill-other-buffers-keep-regexps'."
  (interactive)
  (mapc #'(lambda (buf)
	    (let ((bufname (buffer-name buf)))
	      (unless (cl-find bufname jrm-kill-other-buffers-keep-regexps
			       :test #'(lambda (bn re) (string-match re bn)))
		(kill-buffer buf))))
	(delq (current-buffer) (buffer-list))))
