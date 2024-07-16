;;; Select sexp.

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
