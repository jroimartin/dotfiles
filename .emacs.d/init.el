;;; Initial package setup

(require 'package)

;;;; Custom file

;; Set custom file location
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file)

;;;; User libraries

;; Add user lisp directory to load-path
(add-to-list 'load-path (concat user-emacs-directory "lisp/"))

;; Load libraries
(load-library "jroi-utils")
(load-library "jroi-semlf-mode")

;;;; Packages

;; Add melpa repository
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; Install selected packages if any is missing
(unless (seq-every-p #'package-installed-p package-selected-packages)
  (package-install-selected-packages))

;;; Emacs setup

(require 'flymake)
(require 'org)

;;;; User interface

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
;; directory
(customize-set-variable 'dired-dwim-target t)

;;;; Formatting

;; End sentences with a single space
(customize-set-variable 'sentence-end-double-space nil)

;;;; Dynamic abbreviation

;; Case sensitive search
(customize-set-variable 'dabbrev-case-replace nil)

;;;; Programming languages

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

;; Look for the nearest parent go.mod file as the project root
(defun jroi-project-find-go-module (dir)
  (when-let ((root (locate-dominating-file dir "go.mod")))
    (cons 'go-module root)))

(cl-defmethod project-root ((project (head go-module)))
  (cdr project))

(add-hook 'project-find-functions #'jroi-project-find-go-module)

;; rust-mode
;; Requires: rustup [+toolchain] component add rust-analyzer
(add-hook 'rust-mode-hook
	  #'(lambda ()
	      (eglot-ensure)
	      (add-hook 'before-save-hook #'eglot-format-buffer nil t)))

;; sh-mode
;; Indentation: tabs
(customize-set-variable 'sh-basic-offset tab-width)

;; python-mode
;; Requires: https://github.com/microsoft/pyright
(add-hook 'python-mode-hook #'eglot-ensure)

;; js-mode
;; Indentation: 2 spaces
(customize-set-variable 'js-indent-level 2)
(add-hook 'js-mode-hook
	  #'(lambda ()
	      (setq indent-tabs-mode nil)))

;; sgml-mode
;; SGML standard general markup language, which includes HTML
;; hypertext markup language
;; Indentation: 2 spaces
(customize-set-variable 'sgml-basic-offset 2)
(add-hook 'sgml-mode-hook
	  #'(lambda ()
	      (setq indent-tabs-mode nil)))

;; css-mode
;; Indentation: 2 spaces
(customize-set-variable 'css-indent-offset 2)
(add-hook 'css-mode-hook
	  #'(lambda ()
	      (setq indent-tabs-mode nil)))

;; markdown-mode
;; Requires: go install github.com/jroimartin/mess/md@latest
(customize-set-variable 'markdown-command "md -")

;;;; Org mode

;; Set the default target file for storing notes
(customize-set-variable 'org-default-notes-file (concat org-directory "/inbox.org"))

;;;; Magit

;; Enable forge
(with-eval-after-load 'magit
  (require 'forge))

;; Hide closed topics in forge
(customize-set-variable 'forge-topic-list-limit '(60 . -5))

;;;; Key bindings

;; Select previous window
(global-set-key (kbd "C-c o") #'(lambda ()
				  (interactive)
				  (other-window -1)))

;; Revert current buffer
(global-set-key (kbd "C-c r") #'revert-buffer)

;; Shell
(global-set-key (kbd "C-c s") #'jroi-shell)

;; Select sexp
(global-set-key (kbd "C-c i") #'jroi-select-sexp)

;; Kill other buffers
(global-set-key (kbd "C-c k") #'(lambda ()
				  (interactive)
				  (jroi-kill-other-buffers)
				  (delete-other-windows)))

;; Filename completion
(global-set-key (kbd "C-c f") #'comint-dynamic-complete-filename)

;; flymake
(define-key flymake-mode-map (kbd "M-n") #'flymake-goto-next-error)
(define-key flymake-mode-map (kbd "M-p") #'flymake-goto-prev-error)

;; eglot
(global-set-key (kbd "C-c e r") #'eglot-rename)
(global-set-key (kbd "C-c e e") #'eglot-code-action-extract)
(global-set-key (kbd "C-c e k") #'eglot-shutdown-all)

;;;; Local settings file

;; If it exists and it is readable, this file is loaded at the very
;; end of the init file, after all other initializations and settings
(let ((local-init-file (concat user-emacs-directory "init-local.el")))
  (when (file-readable-p local-init-file)
    (load local-init-file)))
