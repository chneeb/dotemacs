;; Home and vendor path
(setq home-path (expand-file-name "~"))
(setq dotemacs-path (concat home-path "/.emacs.d"))
(setq vendor-path (concat dotemacs-path "/elisp"))

;; Setting up PATH environment variable
(when (equal system-type 'darwin)
  (setenv "PATH" (concat "/opt/local/bin:/usr/local/bin:" (getenv "PATH")))
  (push "/opt/local/bin" exec-path))

;; Setting load path
(add-to-list 'load-path vendor-path)
(progn (cd vendor-path) (normal-top-level-add-subdirs-to-load-path))

;; Basic settings
(blink-cursor-mode 0)
(setq visible-bell t)
(setq inhibit-startup-message t)
(setq frame-title-format "Emacs - %b - %f")
(tool-bar-mode 0)
(transient-mark-mode t)
(global-hl-line-mode 0)
(delete-selection-mode t)
(setq make-backup-files nil)
(prefer-coding-system 'utf-8)
(server-start)

;; paren mode
(setq show-paren-delay 0)
(show-paren-mode t)
(setq show-paren-style 'parenthesis) ;'expression, 'parenthesis or 'mixed

;; Color theme stuff
(require 'color-theme)
(color-theme-initialize)
(color-theme-arjen)

;;(set-frame-parameter (selected-frame) 'alpha '(<active> [<inactive>]))
;;(set-frame-parameter (selected-frame) 'alpha '(85 50))
;;(add-to-list 'default-frame-alist '(alpha 85 50))

;; Turn off anti-aliasing
(setq mac-allow-anti-aliasing nil)

;; BBDB stuff
(setq bbdb-file "~/Dropbox/bbdb")
(require 'bbdb)
(bbdb-initialize)
(setq 
    bbdb-offer-save 1                        ;; 1 means save-without-asking   
    bbdb-use-pop-up t                        ;; allow popups for addresses
    bbdb-electric-p t                        ;; be disposable with SPC
    bbdb-popup-target-lines  1               ;; very small
    bbdb-dwim-net-address-allow-redundancy t ;; always use full name
    bbdb-quiet-about-name-mismatches 2       ;; show name-mismatches 2 secs
    bbdb-always-add-address t                ;; add new addresses to existing...
                                             ;; ...contacts automatically
    bbdb-canonicalize-redundant-nets-p t     ;; x@foo.bar.cx => x@bar.cx
    bbdb-completion-type nil                 ;; complete on anything
    bbdb-complete-name-allow-cycling t       ;; cycle through matches
                                             ;; this only works partially
    bbbd-message-caching-enabled t           ;; be fast
    bbdb-use-alternate-names t               ;; use AKA
    bbdb-elided-display t                    ;; single-line addresses
    ;; auto-create addresses from mail
    bbdb/mail-auto-create-p 'bbdb-ignore-some-messages-hook   
    bbdb-ignore-some-messages-alist ;; don't ask about fake addresses
    ;; NOTE: there can be only one entry per header (such as To, From)
    ;; http://flex.ee.uec.ac.jp/texi/bbdb/bbdb_11.html
    '(( "From" . "no.?reply\\|DAEMON\\|daemon\\|facebookmail\\|twitter")))

;; ido mode
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)
(setq ido-create-new-buffer 'always)

;; Remember and Org-mode stuff
(require 'remember-autoloads)
(setq remember-annotation-functions '(org-remember-annotation))
(setq remember-handler-functions '(org-remember-handler))
(eval-after-load 'remember
  '(add-hook 'remember-mode-hook 'org-remember-apply-template))
(global-set-key (kbd "C-c r") 'remember)

(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(setq org-directory (concat home-path "/Documents/Org"))
(setq org-default-notes-file (concat org-directory "/notes.org"))
(setq org-todo-keywords '("TODO" "DONE"))
(setq org-agenda-include-diary t)
(setq org-agenda-include-all-todo t)
(setq org-hide-leading-stars t)
(setq org-odd-levels-only t)
(setq org-remember-templates
      '(("Todo" ?t "* TODO %? %^g\n %i\n " "todos.org" "Tasks")
	("Journal" ?j "\n* %^{topic} %T \n%i%?\n" "journal.org" "Journal")
	("Private" ?p "\n* %^{topic} %T \n%i%?\n" "private.org" "Diary")
))
(global-set-key (kbd "C-c a") 'org-agenda)

;; W3M Stuff
(require 'w3m)
(setq browse-url-browser-function 'w3m-browse-url)
(setq w3m-use-cookies t)
(autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)
;; optional keyboard short-cut
(global-set-key "\C-xu" 'browse-url-at-point)

;; Ruby stuff
(require 'ruby-mode)
(require 'ruby-electric)
(defun chneeb-ruby-mode-hook()
  (font-lock-mode t)
  (setq standard-indent 2)
  (ruby-electric-mode t))
(add-hook 'ruby-mode-hook 'chneeb-ruby-mode-hook)
(require 'rinari)

;; Clojure
(setq swank-clojure-jar-path "~/.emacs.d/clojure/clojure/clojure.jar"
      swank-clojure-extra-classpaths (list
				      "~/.emacs.d/elisp/swank-clojure/src/main/clojure"
				      "~/.emacs.d/clojure/clojure-contrib/clojure-contrib.jar"))
(require 'clojure-mode)
(require 'swank-clojure-autoload)
(require 'slime)

(eval-after-load "slime" (slime-setup '(slime-repl)))
(slime-setup)

;; OpenSSL
(setq ssl-program-name "openssl")
(setq ssl-program-arguments '("s_client" "-quiet" "-host" host "-port" service))
(setq ssl-certificate-verification-policy 0)

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

;; Twitter
(require 'twitter)
(autoload 'twitter-get-friends-timeline "twitter" nil t)
(autoload 'twitter-status-edit "twitter" nil t)
(global-set-key "\C-xt" 'twitter-get-friends-timeline)
(add-hook 'twitter-status-edit-mode-hook 'longlines-mode)

;; Magit
(require 'magit)

;; Jabber
(require 'jabber-autoloads)
(setq jabber-account-list
    '(("chneeb@gmail.com" 
       (:network-server . "talk.google.com")
       (:connection-type . ssl))))

;; djcb stuff
(load-file (concat dotemacs-path "/djcb.el"))

;; Own stuff
(defun gcal ()
  (interactive)
  (let 
      ((quick-add (read-from-minibuffer "Quick Add: "))
       (max-mini-window-height 0))
    (shell-command (concat "gcal " quick-add) "*gcal*")
    )
  )

;; Try the following
;; - yasnippet
;; - http://orgmode.org/worg/org-customization-guide.php
;; - rubydb (Ruby Debugger)
;; - JDEE
;; - http://www.djcbsoftware.nl/dot-emacs.html
;; - http://www.emacswiki.org/emacs/hgw-init-wl.el
;; - paredit-mode
;; - Exchange Wanderlust IMAP: Try (setq elmo-imap4-default-authenticate-type 'ntlm).
