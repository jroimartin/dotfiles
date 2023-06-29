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
