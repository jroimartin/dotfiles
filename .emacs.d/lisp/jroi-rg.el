;;; Ripgrep integration.

;; Requires: dnf install ripgrep

(defun rg (command-args)
  "Run ripgrep with user-specified COMMAND-ARGS.
The output from the command goes to the \"*grep*\" buffer."
  (interactive
   (list (read-shell-command
          "Run ripgrep (like this): "
          "rg -nH --no-heading -e "
          'rg-history)))
  (compilation-start command-args #'grep-mode))
