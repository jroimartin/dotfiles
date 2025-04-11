;;; jroi-erc.el --- ERC utility functions -*- lexical-binding: t -*-

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

;; ERC utility functions.

;;; Code:

(require 'erc)
(require 'erc-sasl)

(defcustom jroi-erc-accounts-alist nil
  "ERC accounts.

Elements are of the form
(NAME . (:server SERVER :port PORT :nick NICK :user USER :auth MACHINE).

NAME    is the account name passed to `jroi-erc'.
SERVER  is the IRC server.
PORT    is the TLS port of the IRC server.
NICK    is the nick name used to connect to the IRC server.
USER    is the user name used to connect to the IRC server.
MACHINE is the machine field of the corresponding entry in the
        auth-source file (e.g. ~/.authinfo.gpg)."
  :group 'jroi-erc)

(defun jroi-erc (account)
  "Creates a new ERC session with the parameters of the provided
account.  SASL is used for authentication and credentials are
retrieved from `auth-sources'.

ACCOUNT must be an entry of `jroi-erc-accounts-alist'."
  (interactive (list (intern (completing-read "Connect to account: "
					      (mapcar #'car jroi-erc-accounts-alist)
					      nil t))))
  (if-let* ((acc (alist-get account jroi-erc-accounts-alist))
	    (server (plist-get acc :server))
	    (port (plist-get acc :port))
	    (nick (plist-get acc :nick))
	    (user (plist-get acc :user))
	    (auth (plist-get acc :auth)))
      (let ((erc-modules (cons 'sasl erc-modules))
	    (erc-sasl-auth-source-function #'erc-sasl-auth-source-password-as-host))
	(erc-tls :server server :port port :nick nick :user user :password auth))
    (message "Unknown account `%s'" account)))

;;; jroi-erc.el ends here
