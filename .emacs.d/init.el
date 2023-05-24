(require 'package)
(require 'flymake)
(require 'org)

;;; Custom file

;; Set custom file location
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file)

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

;; eglot
(customize-set-variable 'eglot-ignored-server-capabilities '(:inlayHintProvider))

;;; Programming languages

;; go-mode
;; Requires: golang.org/x/tools/gopls
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
(add-hook 'rust-mode-hook
	  #'(lambda ()
	      (eglot-ensure)
	      (add-hook 'before-save-hook #'eglot-format-buffer nil t)))

;; markdown-mode
;; Requires: github.com/jroimartin/mess/md
(customize-set-variable 'markdown-command "md -")

;; shell-script
(customize-set-variable 'sh-basic-offset 8)

;;; Keymaps

;; ansi-term
(global-set-key (kbd "C-c t") #'ansi-term)

;; expand-region
(global-set-key (kbd "C-=") #'er/expand-region)

;; flymake
(define-key flymake-mode-map (kbd "M-n") #'flymake-goto-next-error)
(define-key flymake-mode-map (kbd "M-p") #'flymake-goto-prev-error)

;; org-mode
(global-set-key (kbd "C-c a") #'org-agenda)
