;; gnus
(require 'gnus)
(load-file (concat dotemacs-path "/elisp/gnus/lisp/mailcap.el"))

(setq user-mail-address "chneeb@gmail.com")
(setq user-full-name "Christian Neeb")

;;(setq gnus-fetch-old-headers t)
;;(setq gnus-ignored-newsgroups "")
;;(setq gnus-auto-subscribed-groups "[Gmail]/*")
(setq gnus-use-dribble-file nil)
(setq gnus-summary-line-format "%U%R%z%d %I%(%[ %F %] %s %)\n")

(setq gnus-select-method
      '(nnimap "gmail"
	       (nnimap-address "imap.gmail.com")
	       (nnimap-server-port 993)
	       (nnimap-authinfo-file "~/.authinfo")
               (nnimap-expunge-on-close always)
	       (nnimap-stream ssl)))

;; (setq gnus-secondary-select-methods
;;       '((nnimap "work"
;; 		 (nnimap-address "145.228.237.176")
;; 		 (nnimap-server-port 143))
;; 	(nntp "news.gmane.org")))

(setq smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-default-smtp-server "smtp.gmail.com"
      send-mail-function 'smtpmail-send-it
      message-send-mail-function 'smtpmail-send-it
      smtpmail-smtp-service 587
      smtpmail-auth-credentials '(("smtp.gmail.com"
       587
       "chneeb@gmail.com"
       nil)))

(add-hook 'gnus-topic-mode-hook 'gnus-topic-mode)

;; Import Google Reader OPML file
(require 'greader)
(require 'nnrss)

(defun chneeb/greader-opml-to-string ()
  "Retrieve OPML representation of our subscription list."
  (interactive)
  (declare (special greader-auth-handle greader-subscription-opml-url
                    g-curl-program g-curl-common-options))
  (g-auth-ensure-token greader-auth-handle)
  (shell-command-to-string
   (format
    "%s %s %s %s 2>/dev/null"
    g-curl-program g-curl-common-options
    (g-authorization greader-auth-handle)
    greader-subscription-opml-url)))

(defun chneeb/nnrss-greader-opml-import ()
  (interactive)
  (let ((greader-opml-temp-file (make-temp-file "greader-opml")))
    (with-temp-file greader-opml-temp-file (insert (chneeb/greader-opml-to-string)))
    (nnrss-opml-import greader-opml-temp-file)
    (delete-file greader-opml-temp-file)))
