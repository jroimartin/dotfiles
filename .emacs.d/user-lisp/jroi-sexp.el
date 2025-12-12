;;; jroi-sexp.el --- S-expression utility functions -*- lexical-binding: t -*-

;; Copyright (C) 2025 Roi Martin

;; Author: Roi Martin <jroi.martin@gmail.com>

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; S-expression utility functions.

;;; Code:

(defvar-local jroi-sexp--prev-point nil
  "Point before calling `jroi-sexp-select'.")

;;;###autoload
(defun jroi-sexp-select ()
  "Select the region between the delimiters of the sexp at
point."
  (interactive)
  (let* ((start (save-excursion
		  (backward-up-list nil t t)
		  (point)))
	 (end (save-excursion
		(goto-char start)
		(forward-sexp)
		(point))))
    (setq jroi-sexp--prev-point (point))
    (goto-char (+ start 1))
    (push-mark (- end 1) nil t)))

;;;###autoload
(defun jroi-sexp-restore-point ()
  "Restore point before calling `jroi-sexp-select'."
  (when (and (eq last-command 'jroi-sexp-select)
	     jroi-sexp--prev-point)
    (goto-char jroi-sexp--prev-point)))

;;;###autoload
(advice-add 'keyboard-quit :before #'jroi-sexp-restore-point)

;;; jroi-sexp.el ends here
