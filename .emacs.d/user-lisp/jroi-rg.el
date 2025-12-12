;;; jroi-rg.el --- Ripgrep integration -*- lexical-binding: t -*-

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

;; Ripgrep integration. Requires 'dnf install ripgrep'.

;;; Code:

;;;###autoload
(defun rg (command-args)
  "Run ripgrep with user-specified COMMAND-ARGS.
The output from the command goes to the \"*grep*\" buffer."
  (interactive
   (list (read-shell-command
          "Run ripgrep (like this): "
          "rg -nH --no-heading -e "
          'rg-history)))
  (compilation-start command-args #'grep-mode))

;;; jroi-rg.el ends here
