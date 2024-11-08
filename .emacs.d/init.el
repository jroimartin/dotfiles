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
(load "jroi-erc")
(load "jroi-rg")
(load "jroi-rmail")
(load "jroi-semlf")
(load "jroi-sexp")

;;;; Packages.

(require 'package)

;; Add melpa repository.
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; Install selected packages if any is missing.
(unless (seq-every-p #'package-installed-p package-selected-packages)
  (package-refresh-contents)
  (package-install-selected-packages))

;;; Initial tree-sitter setup.

;; Add configuration for downloading and installing tree-sitter
;; language grammars.
(setq treesit-language-source-alist
      '((go . ("https://github.com/tree-sitter/tree-sitter-go.git" "v0.23.1"))
	(gomod . ("https://github.com/camdencheek/tree-sitter-go-mod.git" "v1.0.2"))
	(rust . ("https://github.com/tree-sitter/tree-sitter-rust.git" "v0.23.0"))
	(dockerfile . ("https://github.com/camdencheek/tree-sitter-dockerfile.git" "v0.2.0"))
	(yaml . ("https://github.com/ikatyang/tree-sitter-yaml.git" "v0.5.0"))
	(c . ("https://github.com/tree-sitter/tree-sitter-c.git" "v0.23.0"))
	(cpp . ("https://github.com/tree-sitter/tree-sitter-cpp.git" "v0.23.0"))))

;; Install missing grammars.
(mapc #'(lambda (lang)
	  (unless (treesit-language-available-p lang)
	    (treesit-install-language-grammar lang)))
      (mapcar #'car treesit-language-source-alist))

;;; Emacs setup.

;;;; Environment.

;; Disable pager.
(setenv "PAGER" "")

;;;; User interface.

;; Disable tool bar.
(tool-bar-mode 0)

;; Disable menu bar.
(menu-bar-mode 0)

;; Disable scroll bar.
(scroll-bar-mode 0)

;; Disable startup screen.
(customize-set-variable 'inhibit-startup-screen t)

;; Disable bell.
(customize-set-variable 'ring-bell-function 'ignore)

;; Show column number.
(customize-set-variable 'column-number-mode t)

;; Do not split windows vertically if possible.
(customize-set-variable 'split-height-threshold nil)

;;;; Faces.

;; Customize fixed-pitch-serif face.
(set-face-attribute 'fixed-pitch-serif nil :family "Go Mono")

;;;; Completion.

;; Switch to the *Completions* window when `completion-at-point' is
;; called twice.
(customize-set-variable 'completion-auto-select 'second-tab)

;;;; Minibuffer completion.

;; Enable Fido mode.
(fido-mode)

;;;; Dynamic abbreviation.

;; Case sensitive search.
(customize-set-variable 'dabbrev-case-replace nil)

;;;; Disabled commands.

;; Convert region to upper and lower case.
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; Narrowing.
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)

;;;; Dired.

;; If there is a Dired buffer displayed in some window, use its
;; current directory, instead of Dired buffer's current directory.
(customize-set-variable 'dired-dwim-target t)

;; Show hidden files and human-readable sizes.
(customize-set-variable 'dired-listing-switches "-lah")

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

;; Define capture templates.
(customize-set-variable 'org-capture-templates
			'(("t" "Task with annotation" entry (file+headline "" "Tasks")
			   "* TODO %?\n  %u\n  %a")
			  ("T" "Task" entry (file+headline "" "Tasks")
			   "* TODO %?\n  %u")))

;; Configure export backends.
(customize-set-variable 'org-export-backends '(ascii html icalendar latex odt md))

;; Enable Babel languages.
(customize-set-variable 'org-babel-load-languages '((emacs-lisp . t)
						    (scheme . t)))

;; Do not show repeated entries in the future part of the agenda.
(customize-set-variable 'org-agenda-show-future-repeats nil)

;; Filter out "someday" and "habit" entries in "Agenda and TODOs"
;; view.
(customize-set-variable 'org-agenda-custom-commands
			'(("n" "Agenda and TODOs"
			   ((agenda "")
			    (tags-todo "-someday-habit")))))

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

;; Performance tweaks for big repositories.

;; Do not show related branches in revision buffers.
(customize-set-variable 'magit-revision-insert-related-refs nil)

;; Do not show tags header in status buffer.
(with-eval-after-load 'magit
  (remove-hook 'magit-status-headers-hook 'magit-insert-tags-header))

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

;;;; Reading mail.

;; Only inline plain text, HTML and images.
(customize-set-variable 'mm-inlined-types '("text/plain" "text/html" "image/.*"))

;;;; Sending mail.

;; Dot not encode utf-8 as base64.
(with-eval-after-load 'mm-bodies
  (add-to-list 'mm-body-charset-encoding-alist '(utf-8 . 8bit)))

;; Use msmtp for sending mails.
(customize-set-variable 'message-send-mail-function #'message-send-mail-with-sendmail)
(customize-set-variable 'message-sendmail-envelope-from 'header)
(customize-set-variable 'sendmail-program (executable-find "msmtp"))

;;;; EWW.

;; Open URLs in EWW by default.
(customize-set-variable 'browse-url-browser-function #'eww-browse-url)

;; Set search engine to DuckDuckGo Lite.
(customize-set-variable 'eww-search-prefix "https://duckduckgo.com/lite/?q=")

;;;; ERC.

;; Enable SASL module.
(customize-set-variable 'erc-modules
			'(autojoin button completion fill irccontrols list match menu move-to-prompt netsplit networks noncommands readonly ring sasl stamp track))

;; Call `erc-auth-source-search' with `erc-sasl-password' as `:host'.
;; It allows to use an auth source to store the credentials of
;; multiple user accounts within the same IRC network.
(customize-set-variable 'erc-sasl-auth-source-function
			#'erc-sasl-auth-source-password-as-host)

;; Bury the buffer created when receiving a new private message.
(customize-set-variable 'erc-auto-query 'bury)

;;;; Project.

;; Try to use the root of a VCS repository as the root of the project.
(add-hook 'project-find-functions #'project-try-vc)

;;;; Eglot.

;; Disable inlay hints.
(customize-set-variable 'eglot-ignored-server-capabilities '(:inlayHintProvider
							     :documentOnTypeFormattingProvider))

;;;; Geiser.

;; Requires: dnf install chez-scheme racket guile30

;; Set default scheme implementation.
(customize-set-variable 'geiser-default-implementation 'chez)

;; Set Guile executable name.
(customize-set-variable 'geiser-guile-binary "guile3.0")

;;;; Programming languages.

;; Enable paren mode.
(show-paren-mode t)
(customize-set-variable 'show-paren-delay 0)

;; C.
;; Requires: dnf install clang-tools-extra
(customize-set-variable 'c-default-style '((java-mode . "java")
					   (awk-mode . "awk")
					   (other . "linux")))
(add-hook 'c-mode-hook #'eglot-ensure)

;; Go.
;; Requires: go install golang.org/x/tools/gopls@latest
(add-to-list 'auto-mode-alist `(,(rx ".go" string-end) . go-ts-mode))
(add-to-list 'auto-mode-alist `(,(rx "/go.mod" string-end) . go-mod-ts-mode))
(add-hook 'go-ts-mode-hook
	  #'(lambda ()
	      (eglot-ensure)
	      (add-hook 'before-save-hook
			#'(lambda ()
			    (call-interactively #'eglot-code-action-organize-imports))
			nil
			t)
	      (add-hook 'before-save-hook #'eglot-format-buffer nil t)))

;; Look for the nearest parent go.mod file as the project root.
(cl-defmethod project-root ((project (head go-module)))
  (cdr project))

(add-hook 'project-find-functions
	  #'(lambda (dir)
	      (when-let ((root (locate-dominating-file dir "go.mod")))
		(cons 'go-module root))))

;; Rust.
;; Requires: rustup [+toolchain] component add rust-analyzer
;; Indentation: 4 spaces
(add-to-list 'auto-mode-alist `(,(rx ".rs" string-end) . rust-ts-mode))
(customize-set-variable 'rust-indent-offset 4)
(add-hook 'rust-ts-mode-hook
	  #'(lambda ()
	      (setq indent-tabs-mode nil)))
(add-hook 'rust-ts-mode-hook
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

;; SGML.  SGML standard general markup language, which includes HTML
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

;; Dockerfile.
(add-to-list 'auto-mode-alist
	     `(,(rx (or (seq "Dockerfile" (? "." (* not-newline)))
			(seq "." (any "Dd") "ockerfile"))
		    string-end)
	       . dockerfile-ts-mode))

;; YAML.
(add-to-list 'auto-mode-alist `(,(rx ".y" (? "a") "ml" string-end) . yaml-ts-mode))

;; WGSL.
;; Indentation: 4 spaces
(autoload 'wgsl-mode "wgsl-mode" "Major mode for editing WGSL code." t)
(add-to-list 'auto-mode-alist `(,(rx ".wgsl" string-end) . wgsl-mode))
(customize-set-variable 'wgsl-mode-basic-offset 4)
(add-hook 'wgsl-mode-hook
	  #'(lambda ()
	      (setq indent-tabs-mode nil)))

;;;; Key bindings.

;; Windmove.
(windmove-default-keybindings)

;; Ibuffer.
(keymap-global-set "C-x C-b" #'ibuffer)

;; Flymake.
(with-eval-after-load 'flymake
  (keymap-set flymake-mode-map "M-n" #'flymake-goto-next-error)
  (keymap-set flymake-mode-map "M-p" #'flymake-goto-prev-error))

;; Eglot.
(keymap-global-set "C-c e r" #'eglot-rename)
(keymap-global-set "C-c e a" #'eglot-code-actions)

;; Select sexp.
(keymap-global-set "C-c i" #'jroi-sexp-select)

;; Notmuch.
(keymap-global-set "C-c m" #'notmuch)

;; Org.
(keymap-global-set "C-c n c" #'org-capture)
(keymap-global-set "C-c n a" #'org-agenda)
(with-eval-after-load 'ol
  (keymap-global-set "C-c n l" #'org-store-link))

;; Denote.
(keymap-global-set "C-c n n" #'denote-region)
(keymap-global-set "C-c n i" #'denote-link)
(keymap-global-set "C-c n I" #'denote-add-links)
(keymap-global-set "C-c n r" #'denote-rename-file)
(keymap-global-set "C-c n R" #'denote-rename-file-using-front-matter)

;; Revert current buffer.
(keymap-global-set "C-c r" #'revert-buffer)

;; Shell.
(keymap-global-set "C-c s" #'shell)

;;;; Local settings file.

;; If it exists and it is readable, this file is loaded at the very
;; end of the init file, after all other initializations and settings.
(let ((local-init-file (concat user-emacs-directory "init-local.el")))
  (when (file-readable-p local-init-file)
    (load local-init-file)))
