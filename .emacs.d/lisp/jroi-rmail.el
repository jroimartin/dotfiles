;;; jroi-rmail.el --- Rmail utility functions -*- lexical-binding: t -*-

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

;; Rmail utility functions.

;;; Code:

(defun jroi-rmail-url (url)
  "Run Rmail on URL."
  (interactive "sRun rmail on URL: ")
  (let ((filename (make-temp-file "url" nil (url-file-extension url))))
    (url-copy-file url filename 'ok-if-already-exists)
    (rmail-input filename)))

;;; jroi-rmail.el ends here
