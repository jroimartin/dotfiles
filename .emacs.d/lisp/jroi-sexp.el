;;; Utilities to work with s-expression.

(defvar-local jroi-sexp--prev-point nil
  "Point before calling `jroi-sexp-select'.")

(defun jroi-sexp-select ()
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
    (setq jroi-sexp--prev-point (point))
    (goto-char (+ start 1))
    (push-mark (- end 1) nil t)))

(defun jroi-sexp--restore-point ()
  "Restore point before calling `jroi-sexp-select'."
  (when (and (eq last-command 'jroi-sexp-select)
	     jroi-sexp--prev-point)
    (goto-char jroi-sexp--prev-point)))

(advice-add 'keyboard-quit :before #'jroi-sexp--restore-point)
