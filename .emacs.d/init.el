(require 'package)
(require 'flymake)
(require 'org)

;;; Custom file

;; Set custom file location
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file)

;;; Local libraries

;; Add site-lisp directory to load-path
(add-to-list 'load-path (concat user-emacs-directory "site-lisp/"))

;; Load libraries
(load-library "jrm-semlf-mode")

;;; Packages

;; Add melpa repository
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;;; User interface

;; Disable tool bar
(tool-bar-mode 0)

;; Disable menu bar
(menu-bar-mode 0)

;; Disable scroll bar
(scroll-bar-mode 0)

;; Disable startup screen
(customize-set-variable 'inhibit-startup-screen t)

;; Set minimum height for splitting windows sensibly
(customize-set-variable 'split-height-threshold 125)

;; Disable bell
(customize-set-variable 'ring-bell-function 'ignore)

;; Show column number
(customize-set-variable 'column-number-mode t)

;; Set a darker region face color
(set-face-attribute 'region nil :background "#eee")

;; Enable paren mode
(show-paren-mode t)
(customize-set-variable 'show-paren-delay 0)

;; Enable fido-mode
(fido-mode)

;; If there is a Dired buffer displayed in some window, use its
;; current directory, instead of this Dired buffer's current
;; directory.
(customize-set-variable 'dired-dwim-target t)

;;; Formatting

;; End sentences with a single space
(customize-set-variable 'sentence-end-double-space nil)

;;; Programming languages

;; eglot
(customize-set-variable 'eglot-ignored-server-capabilities '(:inlayHintProvider))

;; go-mode
;; Requires: go install golang.org/x/tools/gopls@latest
(add-hook 'go-mode-hook
	  #'(lambda ()
	      (eglot-ensure)
	      (add-hook 'before-save-hook
			#'(lambda ()
			    (call-interactively #'eglot-code-action-organize-imports))
			nil
			t)
	      (add-hook 'before-save-hook #'eglot-format-buffer nil t)))

;; rust-mode
;; Requires: rustup [+toolchain] component add rust-analyzer
(add-hook 'rust-mode-hook
	  #'(lambda ()
	      (eglot-ensure)
	      (add-hook 'before-save-hook #'eglot-format-buffer nil t)))

;; python-mode
;; Requires: https://github.com/microsoft/pyright
(add-hook 'python-mode-hook #'eglot-ensure)

;; markdown-mode
;; Requires: go install github.com/jroimartin/mess/md@latest
(customize-set-variable 'markdown-command "md -")

;; shell-script
(customize-set-variable 'sh-basic-offset 8)

;;; Org mode

;; Set the default target file for storing notes
(customize-set-variable 'org-default-notes-file (concat org-directory "/inbox.org"))

;;; Magit

;; Enable forge
(with-eval-after-load 'magit
  (require 'forge))

;;; Keymaps

;; ansi-term
(global-set-key (kbd "C-c t") #'ansi-term)

;; expand-region
(global-set-key (kbd "C-=") #'er/expand-region)

;; flymake
(define-key flymake-mode-map (kbd "M-n") #'flymake-goto-next-error)
(define-key flymake-mode-map (kbd "M-p") #'flymake-goto-prev-error)

;; org-mode
(global-set-key (kbd "C-c o a") #'org-agenda)
(global-set-key (kbd "C-c o c") #'org-capture)
