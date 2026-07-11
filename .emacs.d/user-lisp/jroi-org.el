;;; jroi-org.el --- Utility functions for Org Mode -*- lexical-binding: t -*-

;; Copyright (C) 2026 Roi Martin

;; Author: Roi Martin <jroi.martin@gmail.com>

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

;; Utility functions for Org Mode.

;;; Code:

(require 'org)

(defun jroi-org--sanitize-headline (headline)
  "Sanitize the provided headline so it can be used with IDs."
  (replace-regexp-in-string "[^a-z0-9]" "-" (downcase (string-trim headline))))

;;;###autoload
(defun jroi-org-insert-date-custom-id ()
  "Insert the current date and set the headline's CUSTOM_ID property.
Insert the current date at point formatted as /YYYY-MM-DD/, and set the
current headline's CUSTOM_ID property to YYYYMMDD-headline-text."
  (interactive)
  (when (org-before-first-heading-p)
    (user-error "Point must be after a heading."))
  (let ((now (current-time)))
    (org-set-property "CUSTOM_ID"
		      (concat (format-time-string "%Y%m%d" now) "-"
			      (jroi-org--sanitize-headline (org-get-heading t t t t))))
    (insert (format-time-string "/%Y-%m-%d/" now))))

(provide 'jroi-org)
;;; jroi-org.el ends here
