;;; wgsl-mode.el --- Syntax highlighting for the WebGPU Shading Language -*- lexical-binding: t -*-

;; Copyright (C) 2024 Roi Martin
;; Copyright (C) 2022-2023 Anthony Cowley

;; Authors: 2024- Roi Martin
;;	    2022-2023 Anthony Cowley
;; Keywords: wgsl
;; Version: 1.0.0

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.	 See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Syntax highlighting for the WebGPU Shading Language (WGSL).

;;; Code:

(defconst wgsl-mode--ident-regexp
  (rx symbol-start
      (or alpha (seq "_" (not "_"))) (* (any alphanumeric "_"))
      symbol-end))

(defconst wgsl-mode--keywords-regexp
  (rx symbol-start
      (or "alias" "break" "case" "const" "const_assert" "continue"
	  "continuing" "default" "diagnostic" "discard" "else"
	  "enable" "false" "fn" "for" "if" "let" "loop" "override"
	  "requires" "return" "struct" "switch" "true" "var" "while")
      symbol-end))

(defconst wgsl-mode--attributes-regexp
  (rx "@"
      symbol-start
      (or "align" "binding" "builtin" "compute" "const"
	  "diagnostic" "fragment" "group" "id" "interpolate"
	  "invariant" "location" "must_use" "size" "vertex"
	  "workgroup_size" )
      symbol-end))

(defconst wgsl-mode--builtins-regexp
  (rx "@builtin("
      (group (or "vertex_index" "instance_index" "position"
		 "front_facing" "frag_depth" "sample_index"
		 "sample_mask" "local_invocation_id"
		 "local_invocation_index" "global_invocation_id"
		 "workgroup_id" "num_workgroups"))
      ")"))

(defconst wgsl-mode--access-modes-regexp
  (rx symbol-start
      (or "read" "write" "read_write")
      symbol-end))

(defconst wgsl-mode--address-space-regexp
  (rx symbol-start
      (or "function" "private" "workgroup" "uniform" "storage" "handle")
      symbol-end))

(defconst wgsl-mode--scalar-types-regexp
  (rx symbol-start
      (or "bool" "i32" "u32" "f32" "f16")
      symbol-end))

(defconst wgsl-mode--vec-types-regexp
  (rx symbol-start
      "vec" (or "2" "3" "4")
      symbol-end
      (? "<" (* space) (regexp wgsl-mode--ident-regexp) (* space) ">")))

(defconst wgsl-mode--mat-types-regexp
  (rx symbol-start
      "mat" (or "2" "3" "4") "x" (or "2" "3" "4")
      symbol-end
      (? "<" (* space) (regexp wgsl-mode--ident-regexp) (* space) ">")))

(defconst wgsl-mode--atomic-types-regexp
  (rx symbol-start
      "atomic"
      symbol-end
      (? "<" (* space) (regexp wgsl-mode--scalar-types-regexp) (* space) ">")))

(defconst wgsl-mode--array-types-regexp
  (rx symbol-start
      "array"
      symbol-end
      (? "<"
	 (* space)
	 (or (regexp wgsl-mode--vec-types-regexp)
	     (regexp wgsl-mode--mat-types-regexp)
	     (regexp wgsl-mode--atomic-types-regexp)
	     (regexp wgsl-mode--ident-regexp))
	 (? (* space) "," (* space) (+ digit))
	 (* space)
	 ">")))

(defconst wgsl-mode--struct-types-regexp
  (rx symbol-start
      "struct"
      symbol-end
      (+ space)
      (group (regexp wgsl-mode--ident-regexp))))

(defconst wgsl-mode--var-type-regexp
  (rx ":" (+ space) (group (regexp wgsl-mode--ident-regexp))))

(defconst wgsl-mode--function-name-regexp
  (rx symbol-start
      "fn"
      symbol-end
      (+ space)
      (group (regexp wgsl-mode--ident-regexp))
      (* space)
      "("))

(defconst wgsl-mode--function-call-regexp
  (rx (group (regexp wgsl-mode--ident-regexp))
      (* space)
      "("))

(defconst wgsl-mode--var-name-regexp
  (rx (group (regexp wgsl-mode--ident-regexp))
      (* space)
      (any ":=")))


(defconst wgsl-mode--constant-regexp
  (rx symbol-start
      "const"
      symbol-end
      (+ space)
      (group (regexp wgsl-mode--ident-regexp))))

(defconst wgsl-mode--font-lock-keywords
  `((,wgsl-mode--keywords-regexp . font-lock-keyword-face)
    (,wgsl-mode--attributes-regexp . font-lock-builtin-face)
    (,wgsl-mode--builtins-regexp 1 font-lock-builtin-face)
    (,wgsl-mode--address-space-regexp . font-lock-builtin-face)
    (,wgsl-mode--access-modes-regexp . font-lock-builtin-face)
    (,wgsl-mode--array-types-regexp . font-lock-type-face)
    (,wgsl-mode--vec-types-regexp . font-lock-type-face)
    (,wgsl-mode--mat-types-regexp . font-lock-type-face)
    (,wgsl-mode--atomic-types-regexp . font-lock-type-face)
    (,wgsl-mode--scalar-types-regexp . font-lock-type-face)
    (,wgsl-mode--struct-types-regexp 1 font-lock-type-face)
    (,wgsl-mode--constant-regexp 1 font-lock-constant-face)
    (,wgsl-mode--var-type-regexp 1 font-lock-type-face)
    (,wgsl-mode--var-name-regexp 1 font-lock-variable-name-face)
    (,wgsl-mode--function-name-regexp 1 font-lock-function-name-face)
    (,wgsl-mode--function-call-regexp 1 font-lock-function-call-face)))

(defvar wgsl-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?< "." st)
    (modify-syntax-entry ?> "." st)
    (modify-syntax-entry ?+ "." st)
    (modify-syntax-entry ?- "." st)
    (modify-syntax-entry ?* "." st)
    (modify-syntax-entry ?% "." st)
    (modify-syntax-entry ?\| "." st)
    (modify-syntax-entry ?& "." st)
    (modify-syntax-entry ?= "." st)
    (modify-syntax-entry ?/ ". 124" st)
    (modify-syntax-entry ?* ". 23b" st)
    (modify-syntax-entry ?\n ">" st)
    st)
  "Syntax table used while in `wgsl-mode'.")

(defcustom wgsl-mode-basic-offset 4
  "The indentation of the first non-blank non-comment line."
  :type 'integer
  :safe #'integerp
  :group 'wgsl-mode)

(defun wgsl-mode--indent-line ()
  (let ((bol-depth (save-excursion (car (syntax-ppss (beginning-of-line)))))
	(eol-depth (save-excursion (car (syntax-ppss (end-of-line)))))
	(offset (- (point) (save-excursion (back-to-indentation) (point)))))
    (indent-line-to (* wgsl-mode-basic-offset (min bol-depth eol-depth)))
    (when (> offset 0) (forward-char offset))))

(define-derived-mode wgsl-mode prog-mode "WGSL"
  "Major mode for editing WGSL code."
  (font-lock-add-keywords nil wgsl-mode--font-lock-keywords)
  (setq-local indent-line-function #'wgsl-mode--indent-line)
  (setq-local comment-start "// ")
  (setq-local comment-end "")
  (setq-local comment-use-syntax t)
  (setq-local comment-multi-line t)
  (setq-local electric-indent-chars
	      (append "{}()" electric-indent-chars)))

(provide 'wgsl-mode)
;;; wgsl-mode.el ends here
