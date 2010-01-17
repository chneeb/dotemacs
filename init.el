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
(setq-default indent-tabs-mode nil)

;; paren mode
(setq show-paren-delay 0)
(show-paren-mode t)
(setq show-paren-style 'parenthesis) ;'expression, 'parenthesis or 'mixed

;; Color theme stuff
(require 'color-theme)
(color-theme-initialize)
(color-theme-arjen)

;;(set-frame-parameter (selected-frame) 'alpha '(<active> [<inactive>]))
(set-frame-parameter (selected-frame) 'alpha '(90 90))
(add-to-list 'default-frame-alist '(alpha 90 90))

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
  (set (make-local-variable 'compile-command) (concat "ruby " (buffer-file-name)))
  (local-set-key "\C-c\C-c" 'compile)
  (local-set-key "\C-c\C-d" 'rdebug)
  (ruby-electric-mode t))
(add-hook 'ruby-mode-hook 'chneeb-ruby-mode-hook)
;; Fixing ruby-insert-end
(defun ruby-insert-end () 
  "Insert \"end\" at point and reindent current line." 
  (interactive) 
  (insert "end") 
  (ruby-indent-line t) 
  (end-of-line)) 
(require 'rinari)
(require 'rdebug)

;; Clojure
(setq swank-clojure-jar-path (concat dotemacs-path "/clojure/clojure/clojure.jar")
      swank-clojure-extra-classpaths (list
				      (concat vendor-path "/swank-clojure/src/main/clojure")
				      (concat dotemacs-path "/clojure/clojure-contrib/clojure-contrib.jar")))
(require 'clojure-mode)
(require 'swank-clojure-autoload)
(require 'slime)

(eval-after-load "slime" (slime-setup '(slime-repl)))
(slime-setup)

;; OpenSSL
(setq ssl-program-name "openssl")
(setq ssl-program-arguments '("s_client" "-quiet" "-host" host "-port" service))
(setq ssl-certificate-verification-policy 0)

;; Wanderlust
(load-file (concat dotemacs-path "/wl.el"))

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

;;text-mode
(add-hook 'text-mode-hook
  (lambda() 
    (set-fill-column 78)                    ; lines are 78 chars long ...
    (auto-fill-mode t)                      ; ... and wrapped around 
    (set-input-method "latin-1-prefix")))    ; make " + e => ë etc.

;; Yasnippet
(require 'yasnippet)
(yas/initialize)
(yas/load-directory (concat vendor-path "/yasnippet-0.6.1c/snippets"))

;; gnus
;;(load-file (concat dotemacs-path "/gnus.el"))

;; djcb stuff
;;(load-file (concat dotemacs-path "/djcb.el"))

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
;; - http://orgmode.org/worg/org-customization-guide.php
;; - http://blog.client9.com/2007/09/ruby-mode-for-emacs.html
;; - JDEE
;; - http://www.djcbsoftware.nl/dot-emacs.html
;; - http://www.emacswiki.org/emacs/hgw-init-wl.el
;; - paredit-mode
;; - Exchange Wanderlust IMAP: Try (setq elmo-imap4-default-authenticate-type 'ntlm).
;; - icalendar-import-buffer
;; - http://github.com/eschulte/yasnippets-rails