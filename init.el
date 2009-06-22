;; Home and vendor path
(setq home-path (expand-file-name "~"))
(setq vendor-path (concat home-path "/.emacs.d/elisp"))

;; Basic settings
(blink-cursor-mode 0)
(setq visible-bell t)
(setq inhibit-startup-message t)
(setq frame-title-format "Emacs - %b")
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

;; Setting load path
(add-to-list 'load-path (concat vendor-path))
(add-to-list 'load-path (concat vendor-path "/bbdb-2.35/lisp"))
(add-to-list 'load-path (concat vendor-path "/emacs-w3m-1.4.4"))
(add-to-list 'load-path (concat vendor-path "/color-theme-6.6.0"))
(add-to-list 'load-path (concat vendor-path "/ruby-mode"))
(add-to-list 'load-path (concat vendor-path "/org-6.19b/lisp"))
(add-to-list 'load-path (concat vendor-path "/remember-1.9"))
(add-to-list 'load-path (concat vendor-path "/clojure-mode"))

;; Color theme stuff
(require 'color-theme)
(color-theme-initialize)
(color-theme-robin-hood)

;; BBDB stuff
(require 'bbdb)
(bbdb-initialize)

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
;; (global-set-key "\C-xm" 'browse-url-at-point)

;; Ruby stuff
(require 'ruby-mode)
(require 'ruby-electric)
(defun chneeb-ruby-mode-hook()
  (font-lock-mode t)
  (setq standard-indent 2)
  (ruby-electric-mode t))
(add-hook 'ruby-mode-hook 'chneeb-ruby-mode-hook)

;; Clojure
(require 'clojure-mode)
(setq clojure-src-root "/Users/chneeb/Source")
(setq swank-clojure-extra-classpaths nil)
(clojure-slime-config)

;; Own stuff
(defun gcal ()
  (interactive)
  (let 
      ((quick-add (read-from-minibuffer "Quick Add: "))
       (max-mini-window-height 0))
    (shell-command (concat "ruby '" home-path "/Source/gcal.rb' " quick-add) "*gcal*")
    )
  )

;; Try the following
;; - yasnippet
;; - magit
;; - http://orgmode.org/worg/org-customization-guide.php
;; - rubydb (Ruby Debugger)
;; - tramp
;; - Jabber client
;; - JDEE
;; - eudcb-mab.el
;; - whitespace-mode -- http://www.emacswiki.org/emacs/WhiteSpace
;; - http://www.djcbsoftware.nl/dot-emacs.html
