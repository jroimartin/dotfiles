(defun jrm-shell ()
  "Call `shell', but always prompt for buffer."
  (interactive)
  (let ((current-prefix-arg '(4)))
    (call-interactively #'shell)))

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
