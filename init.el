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
(scroll-bar-mode 0)
(transient-mark-mode t)
(global-hl-line-mode 0)
(delete-selection-mode t)
(setq make-backup-files nil)
(prefer-coding-system 'utf-8)
(server-start)
(setq-default indent-tabs-mode nil)
(global-set-key "\r" 'newline-and-indent) ; auto indent
(setq auto-save-default nil)
(setq compilation-scroll-output t)

;; paren mode
(setq show-paren-delay 0)
(show-paren-mode t)
(setq show-paren-style 'parenthesis) ;'expression, 'parenthesis or 'mixed

;; save session state when you quit emacs
(desktop-save-mode 1)

;; midnight mode purges buffers which haven't been displayed in 3 days
(require 'midnight)
(setq midnight-mode 't)

;; Color theme stuff
;; (load-file (concat dotemacs-path "/zenburn.el"))
;; (require 'color-theme)
;; (color-theme-initialize)
;; (color-theme-zenburn)

;; (set-frame-parameter (selected-frame) 'alpha '(<active> [<inactive>]))
;;(set-frame-parameter (selected-frame) 'alpha '(90 90))
;;(add-to-list 'default-frame-alist '(alpha 90 90))

;; Turn off anti-aliasing
;;(setq mac-allow-anti-aliasing nil)

;;(set-face-attribute 'default nil :height 140)

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
(require 'org-mobile)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(setq org-directory (concat home-path "/Dropbox/Org"))
(setq org-default-notes-file (concat org-directory "/notes.org"))
(setq org-todo-keywords '((sequence "TODO" "|" "DONE")))
(setq org-agenda-include-diary t)
(setq org-agenda-include-all-todo t)
(setq org-hide-leading-stars t)
(setq org-odd-levels-only t)
(setq org-blank-before-new-entry nil)
(setq org-remember-templates
      '(("Todo" ?t "* TODO %? %^g\n %i\n " "todos.org" "Tasks")
	("Journal" ?j "\n* %^{topic} %T \n%i%?\n" "journal.org" "Journal")
	("Private" ?p "\n* %^{topic} %T \n%i%?\n" "private.org" "Diary")
))
(setq org-agenda-files (file-expand-wildcards (concat org-directory "/*.org")))
(setq org-mobile-directory (concat home-path "/Dropbox/MobileOrg/"))
(setq org-mobile-inbox-for-pull (concat org-directory "/inbox.org"))
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
(require 'swank-clojure)
(require 'slime)

(eval-after-load "slime" (slime-setup '(slime-repl)))
(slime-setup)

;; OpenSSL
(setq ssl-program-name "openssl")
(setq ssl-program-arguments '("s_client" "-quiet" "-host" host "-port" service))
(setq ssl-certificate-verification-policy 0)

;; Wanderlust
;;(load-file (concat dotemacs-path "/wl.el"))

;; Twitter
;;(require 'twitter)
;;(autoload 'twitter-get-friends-timeline "twitter" nil t)
;;(autoload 'twitter-status-edit "twitter" nil t)
;;(global-set-key "\C-xt" 'twitter-get-friends-timeline)
;;(add-hook 'twitter-status-edit-mode-hook 'longlines-mode)
(require 'twittering-mode)

;; Magit
(require 'magit)

;; Jabber
(require 'jabber-autoloads)
(setq jabber-account-list
    '(("chneeb@gmail.com" 
       (:network-server . "talk.google.com")
       (:connection-type . ssl))))

;;text-mode
;; (add-hook 'text-mode-hook
;;   (lambda() 
;;     (set-fill-column 78)                    ; lines are 78 chars long ...
;;     (auto-fill-mode t)                      ; ... and wrapped around 
;;     (set-input-method "latin-1-prefix")))    ; make " + e => ë etc.

;; Yasnippet
(require 'yasnippet)
(yas/initialize)
(yas/load-directory (concat vendor-path "/yasnippet-0.6.1c/snippets"))

;; Scala
(require 'scala-mode)
(add-to-list 'auto-mode-alist '("\\.scala$" . scala-mode))
(setq yas/my-directory (concat vendor-path "/scala-mode/contrib/yasnippet/snippets"))
(yas/load-directory yas/my-directory)
(add-hook 'scala-mode-hook
          '(lambda ()
             (yas/minor-mode-on)))

;; Ant
(require 'ant)

;; Atom2RSS
(require 'mm-url)

(defadvice mm-url-insert (after DE-convert-atom-to-rss () )
  "Converts atom to RSS by calling xsltproc."
  (when (re-search-forward "xmlns=\"http://www.w3.org/.*/Atom\"" 
			   nil t)
    (goto-char (point-min))
    (message "Converting Atom to RSS... ")
    (call-process-region (point-min) (point-max) 
			 "xsltproc" 
			 t t nil 
			 (expand-file-name "~/.emacs.d/atom2rss.xsl") "-")
    (goto-char (point-min))
    (message "Converting Atom to RSS... done")))

(ad-activate 'mm-url-insert)

;; google reader opml
(require 'greader)

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

(require 'newsticker)

(setq
;; newsticker-heading-format "%t"
;; newsticker-item-format "%t"
;; newsticker-desc-format "%d\n%c"
;; newsticker-hide-old-items-in-newsticker-buffer t
 newsticker-html-renderer 'w3m-region
;; newsticker-frontend 'newsticker-plainview
;; newsticker-retrieval-interval 0
;; newsticker-use-full-width nil
 newsticker-automatically-mark-items-as-old nil
 newsticker-url-list '(("Clojure and me" "http://clj-me.blogspot.com/feeds/posts/default" nil nil nil)
                      ("disclojure" "http://disclojure.org/feed/" nil nil nil)
                      ("emacs-fu" "http://emacs-fu.blogspot.com/feeds/posts/default" nil nil nil)
                      ("Fast Company" "http://www.fastcompany.com/rss.xml" nil nil nil)
                      ("Gothamist" "http://www.gothamist.com/index.rdf" nil nil nil)
                      ("Hacker News" "http://news.ycombinator.com/rss" nil nil nil)
                      ("Hulu - Playlist for Christian Neeb" "http://www.hulu.com/feed/queue/chneeb" nil nil nil)
                      ("igvita.com" "http://www.igvita.com/feed/" nil nil nil)
                      ("Mostly Maths" "http://www.mostlymaths.net/feeds/posts/default" nil nil nil)
                      ("Nuby on Rails" "http://feeds.feedburner.com/nubyonrails" nil nil nil)
                      ("O'Reilly Deal of the Day" "http://feeds.feedburner.com/oreilly/ebookdealoftheday" nil nil nil)
                      ("OSNews" "http://www.osnews.com/files/recent.rdf" nil nil nil)
                      ("PeepCode Products" "http://feeds.feedburner.com/peepcode/UCzt" nil nil nil)
                      ("Pragmatic Bookshelf" "http://www.pragprog.com/feed/global" nil nil nil)
                      ("Proud to Use Perl" "http://proudtouseperl.com/atom.xml" nil nil nil)
                      ("Rails Inside" "http://feeds.feedburner.com/RailsInside" nil nil nil)
                      ("Railscasts" "http://feeds.feedburner.com/railscasts" nil nil nil)
                      ("Ruby Inside" "http://feeds.feedburner.com/RubyInside" nil nil nil)
                      ("RubyFlow" "http://feeds.feedburner.com/Rubyflow" nil nil nil)
                      ("SPIEGEL ONLINE - Schlagzeilen - Topmeldungen" "http://www.spiegel.de/schlagzeilen/tops/index.rss" nil nil nil)
                      ("Statistical Programming with Clojure" "http://incanter.wordpress.com/feed/" nil nil nil)
                      ("TechCrunch" "http://feeds.feedburner.com/Techcrunch" nil nil nil)
                      ("Zaries's Blog" "http://zaries.wordpress.com/feed/" nil nil nil))
 )

;; Hack overrides marking of items. Useful to update Google Reader but need a way to map item URL to google reader item ID
;; Does not work with plainview mode though because there is no single method or hook after the item gets marked as read
(defun newsticker--treeview-mark-item (item new-age)
  "Mark ITEM with NEW-AGE."
  (when item
    (setcar (nthcdr 4 item) new-age)
    ;; clean up ticker FIXME
    )
  (message (format "Yeah - (%s) - !" item)) ;; Hack!
  (newsticker--cache-save-feed
   (newsticker--cache-get-feed (intern newsticker--treeview-current-feed)))
  (newsticker--treeview-tree-do-update-tags newsticker--treeview-vfeed-tree))

(defun chneeb/newsticker-greader-opml-import ()
  (interactive)
  (let ((greader-opml-temp-file (make-temp-file "greader-opml")))
    (with-temp-file greader-opml-temp-file (insert (chneeb/greader-opml-to-string)))
    (newsticker-opml-import greader-opml-temp-file)
    (delete-file greader-opml-temp-file)))

;; Get feed content from here after sign-in: www.google.com/reader/atom/feed/http://news.ycombinator.com/rss (use greader api)

;; gnus
(load-file (concat dotemacs-path "/gnus.el"))

;; wanderlust
(load-file (concat dotemacs-path "/wl.el"))

;; djcb stuff
;;(load-file (concat dotemacs-path "/djcb.el"))
                                   
;; Own stuff
(defun chneeb/gcal ()
  (interactive)
  (let 
      ((quick-add (read-from-minibuffer "Quick Add: "))
       (max-mini-window-height 0))
    (shell-command (concat "gcal " quick-add) "*gcal*")
    )
  )

(defun chneeb/setantenv ()
  (interactive)
  (let ((ant-home (concat home-path "/Source/adidas/hybris/bin/platform/apache-ant-1.7.1")))
    (setenv "ANT_HOME" ant-home)
    (setenv "PATH" (concat ant-home "/bin:" (getenv "PATH")))))

(defun chneeb/javatags ()
  (interactive)
  (shell-command "find . -name *.java | etags --lang=java -o TAGS -"))

;; Try the following
;; - http://orgmode.org/worg/org-customization-guide.php
;; - http://blog.client9.com/2007/09/ruby-mode-for-emacs.html
;; - JDEE
;; - http://www.djcbsoftware.nl/dot-emacs.html
;; - http://www.emacswiki.org/emacs/hgw-init-wl.el
;; - paredit-mode
;; - icalendar-import-buffer
;; - http://github.com/eschulte/yasnippets-rails
;; - http://julien.danjou.info/google-weather-el.html
;; - Google Maps
;; - http://learn-elisp-for-emacs.org/
;; - https://github.com/espenhw/malabar-mode
;; - http://www.emacswiki.org/emacs-en/AutoComplete
;; - 

;; Ant
(require 'ant)

;; Atom2RSS
(require 'mm-url)

(defadvice mm-url-insert (after DE-convert-atom-to-rss () )
  "Converts atom to RSS by calling xsltproc."
  (when (re-search-forward "xmlns=\"http://www.w3.org/.*/Atom\"" 
			   nil t)
    (goto-char (point-min))
    (message "Converting Atom to RSS... ")
    (call-process-region (point-min) (point-max) 
			 "xsltproc" 
			 t t nil 
			 (expand-file-name "~/.emacs.d/atom2rss.xsl") "-")
    (goto-char (point-min))
    (message "Converting Atom to RSS... done")))

(ad-activate 'mm-url-insert)

;; google reader opml
(require 'greader)

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

(require 'newsticker)

(setq
;; newsticker-heading-format "%t"
;; newsticker-item-format "%t"
;; newsticker-desc-format "%d\n%c"
;; newsticker-hide-old-items-in-newsticker-buffer t
 newsticker-html-renderer 'w3m-region
;; newsticker-frontend 'newsticker-plainview
;; newsticker-retrieval-interval 0
;; newsticker-use-full-width nil
 newsticker-automatically-mark-items-as-old nil
 newsticker-url-list '(("Clojure and me" "http://clj-me.blogspot.com/feeds/posts/default" nil nil nil)
                      ("disclojure" "http://disclojure.org/feed/" nil nil nil)
                      ("emacs-fu" "http://emacs-fu.blogspot.com/feeds/posts/default" nil nil nil)
                      ("Fast Company" "http://www.fastcompany.com/rss.xml" nil nil nil)
                      ("Gothamist" "http://www.gothamist.com/index.rdf" nil nil nil)
                      ("Hacker News" "http://news.ycombinator.com/rss" nil nil nil)
                      ("Hulu - Playlist for Christian Neeb" "http://www.hulu.com/feed/queue/chneeb" nil nil nil)
                      ("igvita.com" "http://www.igvita.com/feed/" nil nil nil)
                      ("Mostly Maths" "http://www.mostlymaths.net/feeds/posts/default" nil nil nil)
                      ("Nuby on Rails" "http://feeds.feedburner.com/nubyonrails" nil nil nil)
                      ("O'Reilly Deal of the Day" "http://feeds.feedburner.com/oreilly/ebookdealoftheday" nil nil nil)
                      ("OSNews" "http://www.osnews.com/files/recent.rdf" nil nil nil)
                      ("PeepCode Products" "http://feeds.feedburner.com/peepcode/UCzt" nil nil nil)
                      ("Pragmatic Bookshelf" "http://www.pragprog.com/feed/global" nil nil nil)
                      ("Proud to Use Perl" "http://proudtouseperl.com/atom.xml" nil nil nil)
                      ("Rails Inside" "http://feeds.feedburner.com/RailsInside" nil nil nil)
                      ("Railscasts" "http://feeds.feedburner.com/railscasts" nil nil nil)
                      ("Ruby Inside" "http://feeds.feedburner.com/RubyInside" nil nil nil)
                      ("RubyFlow" "http://feeds.feedburner.com/Rubyflow" nil nil nil)
                      ("SPIEGEL ONLINE - Schlagzeilen - Topmeldungen" "http://www.spiegel.de/schlagzeilen/tops/index.rss" nil nil nil)
                      ("Statistical Programming with Clojure" "http://incanter.wordpress.com/feed/" nil nil nil)
                      ("TechCrunch" "http://feeds.feedburner.com/Techcrunch" nil nil nil)
                      ("Zaries's Blog" "http://zaries.wordpress.com/feed/" nil nil nil))
 )

;; Hack overrides marking of items. Useful to update Google Reader but need a way to map item URL to google reader item ID
;; Does not work with plainview mode though because there is no single method or hook after the item gets marked as read
(defun newsticker--treeview-mark-item (item new-age)
  "Mark ITEM with NEW-AGE."
  (when item
    (setcar (nthcdr 4 item) new-age)
    ;; clean up ticker FIXME
    )
  (message (format "Yeah - (%s) - !" item)) ;; Hack!
  (newsticker--cache-save-feed
   (newsticker--cache-get-feed (intern newsticker--treeview-current-feed)))
  (newsticker--treeview-tree-do-update-tags newsticker--treeview-vfeed-tree))

(defun chneeb/newsticker-greader-opml-import ()
  (interactive)
  (let ((greader-opml-temp-file (make-temp-file "greader-opml")))
    (with-temp-file greader-opml-temp-file (insert (chneeb/greader-opml-to-string)))
    (newsticker-opml-import greader-opml-temp-file)
    (delete-file greader-opml-temp-file)))

;; Get feed content from here after sign-in: www.google.com/reader/atom/feed/http://news.ycombinator.com/rss (use greader api)

;; gnus
(load-file (concat dotemacs-path "/gnus.el"))

;; wanderlust
(load-file (concat dotemacs-path "/wl.el"))

;; djcb stuff
;;(load-file (concat dotemacs-path "/djcb.el"))
                                   
;; Own stuff
(defun chneeb/gcal ()
  (interactive)
  (let 
      ((quick-add (read-from-minibuffer "Quick Add: "))
       (max-mini-window-height 0))
    (shell-command (concat "gcal " quick-add) "*gcal*")
    )
  )

(defun chneeb/setantenv ()
  (interactive)
  (let ((ant-home (concat home-path "/Source/adidas/hybris/bin/platform/apache-ant-1.7.1")))
    (setenv "ANT_HOME" ant-home)
    (setenv "PATH" (concat ant-home "/bin:" (getenv "PATH")))))

(defun chneeb/javatags ()
  (interactive)
  (shell-command "find . -name *.java | etags --lang=java -o TAGS -"))

;; Try the following
;; - http://orgmode.org/worg/org-customization-guide.php
;; - http://blog.client9.com/2007/09/ruby-mode-for-emacs.html
;; - JDEE
;; - http://www.djcbsoftware.nl/dot-emacs.html
;; - http://www.emacswiki.org/emacs/hgw-init-wl.el
;; - paredit-mode
;; - icalendar-import-buffer
;; - http://github.com/eschulte/yasnippets-rails
;; - http://julien.danjou.info/google-weather-el.html
;; - Google Maps
;; - http://learn-elisp-for-emacs.org/
;; - https://github.com/espenhw/malabar-mode
;; - http://www.emacswiki.org/emacs-en/AutoComplete
;; - 

;; Ant
(require 'ant)

;; Atom2RSS
(require 'mm-url)

(defadvice mm-url-insert (after DE-convert-atom-to-rss () )
  "Converts atom to RSS by calling xsltproc."
  (when (re-search-forward "xmlns=\"http://www.w3.org/.*/Atom\"" 
			   nil t)
    (goto-char (point-min))
    (message "Converting Atom to RSS... ")
    (call-process-region (point-min) (point-max) 
			 "xsltproc" 
			 t t nil 
			 (expand-file-name "~/.emacs.d/atom2rss.xsl") "-")
    (goto-char (point-min))
    (message "Converting Atom to RSS... done")))

(ad-activate 'mm-url-insert)

;; google reader opml
(require 'greader)

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

(require 'newsticker)

(setq
;; newsticker-heading-format "%t"
;; newsticker-item-format "%t"
;; newsticker-desc-format "%d\n%c"
;; newsticker-hide-old-items-in-newsticker-buffer t
 newsticker-html-renderer 'w3m-region
;; newsticker-frontend 'newsticker-plainview
;; newsticker-retrieval-interval 0
;; newsticker-use-full-width nil
 newsticker-automatically-mark-items-as-old nil
 newsticker-url-list '(("Clojure and me" "http://clj-me.blogspot.com/feeds/posts/default" nil nil nil)
                      ("disclojure" "http://disclojure.org/feed/" nil nil nil)
                      ("emacs-fu" "http://emacs-fu.blogspot.com/feeds/posts/default" nil nil nil)
                      ("Fast Company" "http://www.fastcompany.com/rss.xml" nil nil nil)
                      ("Gothamist" "http://www.gothamist.com/index.rdf" nil nil nil)
                      ("Hacker News" "http://news.ycombinator.com/rss" nil nil nil)
                      ("Hulu - Playlist for Christian Neeb" "http://www.hulu.com/feed/queue/chneeb" nil nil nil)
                      ("igvita.com" "http://www.igvita.com/feed/" nil nil nil)
                      ("Mostly Maths" "http://www.mostlymaths.net/feeds/posts/default" nil nil nil)
                      ("Nuby on Rails" "http://feeds.feedburner.com/nubyonrails" nil nil nil)
                      ("O'Reilly Deal of the Day" "http://feeds.feedburner.com/oreilly/ebookdealoftheday" nil nil nil)
                      ("OSNews" "http://www.osnews.com/files/recent.rdf" nil nil nil)
                      ("PeepCode Products" "http://feeds.feedburner.com/peepcode/UCzt" nil nil nil)
                      ("Pragmatic Bookshelf" "http://www.pragprog.com/feed/global" nil nil nil)
                      ("Proud to Use Perl" "http://proudtouseperl.com/atom.xml" nil nil nil)
                      ("Rails Inside" "http://feeds.feedburner.com/RailsInside" nil nil nil)
                      ("Railscasts" "http://feeds.feedburner.com/railscasts" nil nil nil)
                      ("Ruby Inside" "http://feeds.feedburner.com/RubyInside" nil nil nil)
                      ("RubyFlow" "http://feeds.feedburner.com/Rubyflow" nil nil nil)
                      ("SPIEGEL ONLINE - Schlagzeilen - Topmeldungen" "http://www.spiegel.de/schlagzeilen/tops/index.rss" nil nil nil)
                      ("Statistical Programming with Clojure" "http://incanter.wordpress.com/feed/" nil nil nil)
                      ("TechCrunch" "http://feeds.feedburner.com/Techcrunch" nil nil nil)
                      ("Zaries's Blog" "http://zaries.wordpress.com/feed/" nil nil nil))
 )

;; Hack overrides marking of items. Useful to update Google Reader but need a way to map item URL to google reader item ID
;; Does not work with plainview mode though because there is no single method or hook after the item gets marked as read
(defun newsticker--treeview-mark-item (item new-age)
  "Mark ITEM with NEW-AGE."
  (when item
    (setcar (nthcdr 4 item) new-age)
    ;; clean up ticker FIXME
    )
  (message (format "Yeah - (%s) - !" item)) ;; Hack!
  (newsticker--cache-save-feed
   (newsticker--cache-get-feed (intern newsticker--treeview-current-feed)))
  (newsticker--treeview-tree-do-update-tags newsticker--treeview-vfeed-tree))

(defun chneeb/newsticker-greader-opml-import ()
  (interactive)
  (let ((greader-opml-temp-file (make-temp-file "greader-opml")))
    (with-temp-file greader-opml-temp-file (insert (chneeb/greader-opml-to-string)))
    (newsticker-opml-import greader-opml-temp-file)
    (delete-file greader-opml-temp-file)))

;; Get feed content from here after sign-in: www.google.com/reader/atom/feed/http://news.ycombinator.com/rss (use greader api)

;; gnus
(load-file (concat dotemacs-path "/gnus.el"))

;; wanderlust
(load-file (concat dotemacs-path "/wl.el"))

;; djcb stuff
;;(load-file (concat dotemacs-path "/djcb.el"))
                                   
;; Own stuff
(defun chneeb/gcal ()
  (interactive)
  (let 
      ((quick-add (read-from-minibuffer "Quick Add: "))
       (max-mini-window-height 0))
    (shell-command (concat "gcal " quick-add) "*gcal*")
    )
  )

(defun chneeb/setantenv ()
  (interactive)
  (let ((ant-home (concat home-path "/Source/adidas/hybris/bin/platform/apache-ant-1.7.1")))
    (setenv "ANT_HOME" ant-home)
    (setenv "PATH" (concat ant-home "/bin:" (getenv "PATH")))))

(defun chneeb/javatags ()
  (interactive)
  (shell-command "find . -name *.java | etags --lang=java -o TAGS -"))

;; Try the following
;; - http://orgmode.org/worg/org-customization-guide.php
;; - http://blog.client9.com/2007/09/ruby-mode-for-emacs.html
;; - JDEE
;; - http://www.djcbsoftware.nl/dot-emacs.html
;; - http://www.emacswiki.org/emacs/hgw-init-wl.el
;; - paredit-mode
;; - icalendar-import-buffer
;; - http://github.com/eschulte/yasnippets-rails
;; - http://julien.danjou.info/google-weather-el.html
;; - Google Maps
;; - http://learn-elisp-for-emacs.org/
;; - https://github.com/espenhw/malabar-mode
;; - http://www.emacswiki.org/emacs-en/AutoComplete
;; - http://jawher.wordpress.com/2011/01/17/scala-development-environment-emacs-sbt-ensime/
;; - https://github.com/RayRacine/scallap/blob/master/tools/emacs/sbt.el