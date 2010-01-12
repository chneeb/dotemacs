;; gnus
(require 'gnus)

(setq user-mail-address "chneeb@gmail.com")
(setq user-full-name "Christian Neeb")

(setq gnus-select-method
      '(nnimap "gmail"
	       (nnimap-address "imap.gmail.com")
	       (nnimap-server-port 993)
	       (nnimap-authinfo-file "~/.authinfo")
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
