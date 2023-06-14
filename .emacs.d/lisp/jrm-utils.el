(defun jrm-shell ()
  "Call `shell', but always prompt for buffer."
  (interactive)
  (let ((current-prefix-arg '(4)))
    (call-interactively #'shell)))
