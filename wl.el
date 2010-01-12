;; wanderlust
(autoload 'wl "wl" "Wanderlust" t)
(autoload 'wl-other-frame "wl" "Wanderlust on new frame." t)
(autoload 'wl-draft "wl-draft" "Write draft with Wanderlust." t)
 
;; Fields in the e-mail header that I do not want to see (regexps)
(setq wl-message-ignored-field-list (quote (".*Received:" ".*Path:" ".*Id:" "^References:" "^Replied:" "^Errors-To:" "^Lines:" "^Sender:" ".*Host:" "^Xref:" "^Content-Type:" "^Precedence:" "^Status:" "^X-VM-.*:" "^List-*" "^Authentication-Results*" "^X-*" "^Received-SPF*" "^DKIM-Signature:" "^DomainKey-Signature:" "^X-Mailman-Version:")))
;; Fields in the e-mail header that I want to see even if they match the regex in wl-message-ignored-field-list
(setq wl-message-visible-field-list (quote ("^Dnas.*:" "^Message-Id:" "^X-Mailer:" "^X-Mailman-Version:")))
 
;; IMAP
(setq elmo-imap4-default-server "imap.gmail.com")
(setq elmo-imap4-default-user "chneeb@gmail.com")
(setq elmo-imap4-default-authenticate-type 'clear)
(setq elmo-imap4-default-port '993)
(setq elmo-imap4-default-stream-type 'ssl)
(setq elmo-imap4-use-modified-utf7 t)

;; SMTP
(setq wl-smtp-connection-type 'starttls)
(setq wl-smtp-posting-port 587)
(setq wl-smtp-authenticate-type "plain")
(setq wl-smtp-posting-user "chneeb")
(setq wl-smtp-posting-server "smtp.gmail.com")
(setq wl-local-domain "gmail.com")
 
(setq wl-from "Christian Neeb <chneeb@gmail.com>")
(setq wl-draft-enable-queuing t)
(if (eq window-system 'mac)
    (setq wl-stay-folder-window t))
(setq wl-default-folder "%inbox")
(setq wl-default-spec "%")
(setq wl-draft-folder "%[Gmail]/Drafts") ; Gmail IMAP
(setq wl-trash-folder "%[Gmail]/Trash")
 
(setq wl-folder-check-async t)
 
(setq elmo-imap4-use-modified-utf7 t)
 
(autoload 'wl-user-agent-compose "wl-draft" nil t)
(if (boundp 'mail-user-agent)
    (setq mail-user-agent 'wl-user-agent))
(if (fboundp 'define-mail-user-agent)
    (define-mail-user-agent
      'wl-user-agent
      'wl-user-agent-compose
      'wl-draft-send
      'wl-draft-kill
      'mail-send-hook))
 
(setq wl-folders-file (concat dotemacs-path "/wl-folders"))

(require 'bbdb-wl)
(bbdb-wl-setup)

;; i don't want to store addresses from my mailing folders
;; (setq 
;;  bbdb-wl-folder-regexp    ;; get addresses only from these folders
;;  "^\.inbox$\\|^.sent")    ;; 
;; (define-key wl-draft-mode-map (kbd "<C-tab>") 'bbdb-complete-name)
