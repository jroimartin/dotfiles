;;; Custom file

;; Set custom file location
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file)

;;; Packages

;;;; Repositories
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;;;; Configurations

;; expand-region
(global-set-key (kbd "C-=") #'er/expand-region)

;; fido-mode
(fido-mode)

;; flymake
(require 'flymake)
(define-key flymake-mode-map (kbd "M-n") #'flymake-goto-next-error)
(define-key flymake-mode-map (kbd "M-p") #'flymake-goto-prev-error)

;; markdown-mode
;; Requires: github.com/jroimartin/mess/md
(setq markdown-command "md -")

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

;;; User interface

;; Disable tool bar
(tool-bar-mode 0)

;; Disable menu bar
(menu-bar-mode 0)

;; Disable scroll bar
(scroll-bar-mode 0)

;; Disable startup screen
(setq inhibit-startup-screen t)

;; Disable bell
(setq ring-bell-function 'ignore)

;; Show column number
(setq column-number-mode t)

;; Set a darker region face color
(set-face-attribute 'region nil :background "#eee")

;; Enable paren mode
(show-paren-mode t)
(setq show-paren-delay 0)

;; Set minimum height for splitting windows sensibly
(setq split-height-threshold 125)

;;; Keymaps
(global-set-key (kbd "C-c t") 'ansi-term)
