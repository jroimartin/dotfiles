;;; Initial package setup.

;;;; Custom file.

;; Set custom file location.
(setq custom-file (concat user-emacs-directory "custom.el"))

;; Load custom file.
(load custom-file)

;;;; User libraries.

;; Add user lisp directory to load-path.
(add-to-list 'load-path (concat user-emacs-directory "lisp/"))

;; Load libraries.
(load "jroi-utils")
(load "jroi-semlf")

;;;; Packages.

(require 'package)

;; Add melpa repository.
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; Install selected packages if any is missing.
(unless (seq-every-p #'package-installed-p package-selected-packages)
  (package-install-selected-packages))

;;; Emacs setup.

;;;; User interface.

;; Disable tool bar.
(tool-bar-mode 0)

;; Disable menu bar.
(menu-bar-mode 0)

;; Disable scroll bar.
(scroll-bar-mode 0)

;; Disable startup screen.
(customize-set-variable 'inhibit-startup-screen t)

;; Set minimum height for splitting windows sensibly.
(customize-set-variable 'split-height-threshold 125)

;; Disable bell.
(customize-set-variable 'ring-bell-function 'ignore)

;; Show column number.
(customize-set-variable 'column-number-mode t)

;; Enable paren mode.
(show-paren-mode t)
(customize-set-variable 'show-paren-delay 0)

;; Enable Fido vertical mode.
(fido-vertical-mode)

;;;; Faces.

;; Set a darker region face color.
(set-face-attribute 'region nil :background "#eee")

;; Customize fixed-pitch-serif face.
(set-face-attribute 'fixed-pitch-serif nil :family "Go Mono")

;;;; Dynamic abbreviation.

;; Case sensitive search.
(customize-set-variable 'dabbrev-case-replace nil)

;;;; Dired.

;; If there is a Dired buffer displayed in some window, use its
;; current directory, instead of Dired buffer's current directory.
(customize-set-variable 'dired-dwim-target t)

;;;; Compilation.

;; Try to translate SGR control sequences into text properties.
(customize-set-variable 'ansi-color-for-compilation-mode t)
(add-hook 'compilation-filter-hook #'ansi-color-compilation-filter)

;;;; ElDoc.

;; Do not allow long ElDoc doc strings to resize echo area display.
(customize-set-variable 'eldoc-echo-area-use-multiline-p nil)

;;;; Org.

;; Set the default target file for storing notes.
(customize-set-variable 'org-directory "~/org/")
(customize-set-variable 'org-default-notes-file (concat org-directory "/inbox.org"))
(customize-set-variable 'org-agenda-files (concat org-directory "/agenda-files"))

;; Capture templates.
(customize-set-variable 'org-capture-templates
			'(("t" "Task" entry (file+headline "" "Tasks")
			   "* TODO %?\n  %u\n  %a")))

;;;; Denote.

;; Set default notes directory.
(customize-set-variable 'denote-directory (expand-file-name "~/notes/"))

;; Empty keywords list.
(customize-set-variable 'denote-known-keywords nil)

;; Add denote template to org-capture.
(with-eval-after-load 'org-capture
  (add-to-list 'org-capture-templates
	       '("n" "Note (with Denote)" plain
		 (file denote-last-path)
		 #'denote-org-capture)))

;; Fontify all Denote-style file names in dired.
(add-hook 'dired-mode-hook #'denote-dired-mode)

;;;; Magit.

;; Enable forge.
(with-eval-after-load 'magit
  (require 'forge))

;; Hide closed topics in forge.
(customize-set-variable 'forge-topic-list-limit '(60 . -5))

;; Show word-granularity differences within diff hunks.
(customize-set-variable 'magit-diff-refine-hunk t)

;;;; Notmuch.

;; Requires: dnf install notmuch emacs-notmuch

;; Autoload notmuch.
(autoload 'notmuch "notmuch" "notmuch mail" t)

;; Enable ol-notmuch.
(with-eval-after-load 'notmuch
  (require 'ol-notmuch))

;; Sections for notmuch-hello.
(customize-set-variable 'notmuch-hello-sections '(notmuch-hello-insert-saved-searches
						  notmuch-hello-insert-alltags))

;; Show all tags in notmuch-hello.
(customize-set-variable 'notmuch-show-all-tags-list t)

;; Show the newest mail first when searching.
(customize-set-variable 'notmuch-search-oldest-first nil)

;;;; Sending mail.

;; Insert CC and BCC headers.
(customize-set-variable 'message-default-headers "CC: \nBCC: \n")

;; Dot not encode utf-8 as base64.
(with-eval-after-load 'mm-bodies
  (add-to-list 'mm-body-charset-encoding-alist '(utf-8 . 8bit)))

;; Use msmtp for sending mails.
(customize-set-variable 'message-send-mail-function #'message-send-mail-with-sendmail)
(customize-set-variable 'message-sendmail-envelope-from 'header)
(customize-set-variable 'sendmail-program (executable-find "msmtp"))

;;;; EWW.

;; Set search engine to DuckDuckGo Lite.
(customize-set-variable 'eww-search-prefix "https://duckduckgo.com/lite/?q=")

;;;; Eglot.

;; Disable inlay hints.
(customize-set-variable 'eglot-ignored-server-capabilities '(:inlayHintProvider))

;;;; Programming languages.

;; Go.
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

;; Look for the nearest parent go.mod file as the project root.
(defun jroi-project-find-go-module (dir)
  (when-let ((root (locate-dominating-file dir "go.mod")))
    (cons 'go-module root)))

(cl-defmethod project-root ((project (head go-module)))
  (cdr project))

(add-hook 'project-find-functions #'jroi-project-find-go-module)

;; Rust.
;; Requires: rustup [+toolchain] component add rust-analyzer
;; Indentation: 4 spaces
(customize-set-variable 'rust-indent-offset 4)
(add-hook 'rust-mode-hook
	  #'(lambda ()
	      (setq indent-tabs-mode nil)))
(add-hook 'rust-mode-hook
	  #'(lambda ()
	      (eglot-ensure)
	      (add-hook 'before-save-hook #'eglot-format-buffer nil t)))

;; Shell.
;; Indentation: tabs
(customize-set-variable 'sh-basic-offset tab-width)

;; Python.
;; Requires: https://github.com/microsoft/pyright
(add-hook 'python-mode-hook #'eglot-ensure)

;; JavaScript.
;; Indentation: 2 spaces
(customize-set-variable 'js-indent-level 2)
(add-hook 'js-mode-hook
	  #'(lambda ()
	      (setq indent-tabs-mode nil)))

;; SGML. SGML standard general markup language, which includes HTML
;; hypertext markup language.
;; Indentation: 2 spaces
(customize-set-variable 'sgml-basic-offset 2)
(add-hook 'sgml-mode-hook
	  #'(lambda ()
	      (setq indent-tabs-mode nil)))

;; CSS.
;; Indentation: 2 spaces
(customize-set-variable 'css-indent-offset 2)
(add-hook 'css-mode-hook
	  #'(lambda ()
	      (setq indent-tabs-mode nil)))

;; Markdown.
;; Requires: go install github.com/jroimartin/mess/md@latest
(customize-set-variable 'markdown-command "md -")

;;;; Key bindings.

;; Ibuffer.
(keymap-global-set "C-x C-b" #'ibuffer)

;; Completion.
(keymap-set completion-in-region-mode-map "M-n" #'minibuffer-next-completion)
(keymap-set completion-in-region-mode-map "M-p" #'minibuffer-previous-completion)

;; Flymake.
(with-eval-after-load 'flymake
  (keymap-set flymake-mode-map "M-n" #'flymake-goto-next-error)
  (keymap-set flymake-mode-map "M-p" #'flymake-goto-prev-error))

;; Eglot.
(keymap-global-set "C-c e r" #'eglot-rename)
(keymap-global-set "C-c e a" #'eglot-code-actions)

;; Filename completion.
(keymap-global-set "C-c f" #'comint-dynamic-complete-filename)

;; Select sexp.
(keymap-global-set "C-c i" #'jroi-select-sexp)

;; Notmuch.
(keymap-global-set "C-c m" #'notmuch)

;; Org.
(keymap-global-set "C-c n c" #'org-capture)
(keymap-global-set "C-c n a" #'org-agenda)
(keymap-global-set "C-c n l" #'org-store-link)

;; Denote.
(keymap-global-set "C-c n n" #'denote-region)
(keymap-global-set "C-c n i" #'denote-link)
(keymap-global-set "C-c n I" #'denote-add-links)
(keymap-global-set "C-c n r" #'denote-rename-file)
(keymap-global-set "C-c n R" #'denote-rename-file-using-front-matter)

;; Select previous window.
(keymap-global-set "C-c o" #'(lambda ()
			       (interactive)
			       (other-window -1)))

;; Revert current buffer.
(keymap-global-set "C-c r" #'revert-buffer)

;; Shell.
(keymap-global-set "C-c s" #'jroi-shell)

;;;; Local settings file.

;; If it exists and it is readable, this file is loaded at the very
;; end of the init file, after all other initializations and settings.
(let ((local-init-file (concat user-emacs-directory "init-local.el")))
  (when (file-readable-p local-init-file)
    (load local-init-file)))
