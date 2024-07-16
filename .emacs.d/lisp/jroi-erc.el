;;; ERC utils.

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
account.

ACCOUNT must be an entry of `jroi-erc-accounts-alist'."
  (interactive "SConnect to account: ")
  (if-let ((acc (alist-get account jroi-erc-accounts-alist))
	   (server (plist-get acc :server))
	   (port (plist-get acc :port))
	   (nick (plist-get acc :nick))
	   (user (plist-get acc :user))
	   (auth (plist-get acc :auth)))
      (erc-tls :server server :port port :nick nick :user user :password auth)
    (message "Unknown account `%s'" account)))
