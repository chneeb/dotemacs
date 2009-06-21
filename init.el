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

;; Convenience keys
;; (global-set-key [f4] 'kill-this-buffer)
;; (global-set-key [f6] 'other-window)
;; (global-set-key [f7] 'delete-other-windows)
;; (global-set-key (kbd "<C-tab>") 'bury-buffer)

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
(add-to-list 'load-path (concat vendor-path "/slime"))
(add-to-list 'load-path (concat vendor-path "/ecb-2.32"))
(add-to-list 'load-path (concat vendor-path "/tramp-2.1.15/lisp"))
(add-to-list 'load-path (concat vendor-path "/clojure-mode"))
(add-to-list 'load-path (concat vendor-path "/swank-clojure"))
(add-to-list 'load-path (concat vendor-path "/emacs-nav-28"))
(add-to-list 'load-path "/usr/local/scala/misc/scala-tool-support/emacs")

;; Scala mode
(require 'scala-mode-auto)

;; Malyon -- Infocom interpreter
(require 'malyon)

;; CEDET
(load-file (concat vendor-path "/cedet-1.0pre6/common/cedet.el"))
(global-ede-mode 1)                      ; Enable the Project management system
(semantic-load-enable-code-helpers)      ; Enable prototype help and smart completion 
(global-srecode-minor-mode 1)            ; Enable template insertion menu

;; Nav
(require 'nav)

;; Tramp
(require 'tramp)
(setq tramp-default-method "scp")

;; ECB
(require 'ecb-autoloads)

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

;; iTunes control
;;  * Press the itunes key and then one of:
;;      key again            : toggle info window.
;;      +/-/up/down/mute/0-9 : volume control
;;      left/right           : prev/next track
;;      space                : toggles playing
;;      </>                  : increase/decrease rating by one star
(setq itunes-key [f5])
(if (string-match "darwin" system-configuration)
    (require 'osx-itunes)
  )

;; twitter.el -- http://emacs-fu.blogspot.com/2009/03/twitter.html
(autoload 'twitter-get-friends-timeline "twitter" nil t)
(autoload 'twitter-status-edit "twitter" nil t)
(global-set-key "\C-xt" 'twitter-get-friends-timeline)
(add-hook 'twitter-status-edit-mode-hook 'longlines-mode)

;; Ruby stuff
(autoload 'ruby-mode "ruby-mode" "Major mode for editing ruby scripts." t)
(setq auto-mode-alist  (cons '(".rb$" . ruby-mode) auto-mode-alist))
(setq auto-mode-alist  (cons '(".rhtml$" . html-mode) auto-mode-alist))

(add-hook 'ruby-mode-hook
          (lambda()
            (add-hook 'local-write-file-hooks
                      '(lambda()
                         (save-excursion
                           (untabify (point-min) (point-max))
                           (delete-trailing-whitespace)
                           )))
            (set (make-local-variable 'indent-tabs-mode) 'nil)
            (set (make-local-variable 'tab-width) 2)
            (imenu-add-to-menubar "IMENU")
	    ;;            (define-key ruby-mode-map "C-m" 'newline-and-indent) ;Not sure if this line is 100% right but it works!
            (require 'ruby-electric)
            (ruby-electric-mode t)
            ))
(add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))
(autoload 'run-ruby "inf-ruby"
  "Run an inferior Ruby process")
(autoload 'inf-ruby-keys "inf-ruby"
  "Set local key defs for inf-ruby in ruby-mode")
(add-hook 'ruby-mode-hook
	  '(lambda ()
	     (inf-ruby-keys)))
;; If you have Emacs 19.2x or older, use rubydb2x                              
(autoload 'rubydb "rubydb3x" "Ruby debugger" t)

(defun ruby-eval-buffer () (interactive)
  "Evaluate the buffer with ruby."
  (shell-command-on-region (point-min) (point-max) "ruby"))

;; Anything.el
;; This is only an example. Customize it to your own taste!
(defvar anything-sources `(((name . "Buffers")
                            (candidates . anything-buffer-list)
                            (action . (("Switch to Buffer" . switch-to-buffer)
                                       ("Kill Buffer" . kill-buffer))))

                           ((name . "File Name History")
                            (candidates . file-name-history)
                            (action . find-file)
                            (type . file))

                           ((name . "Files from Current Directory")
                            (init-func . (lambda ()
                                           (setq anything-default-directory
                                                 default-directory)))
                            (candidates . (lambda ()
                                            (directory-files
                                             anything-default-directory)))
                            (action . find-file)
                            (type . file))

                           ((name . "Manual Pages")
                            (candidates . ,(progn
                                             (require 'woman)
                                             (woman-file-name "")
                                             (sort (mapcar 'car
                                                           woman-topic-all-completions)
                                                   'string-lessp)))
                            (action . woman)
                            (requires-pattern . 2))

                           ((name . "Complex Command History")
                            (candidates . (lambda ()
                                            (mapcar 'prin1-to-string
                                                    command-history)))
                            (action . (lambda (c)
                                        (eval (read c))))
                            (delayed))))
(require 'anything)
(global-set-key "\C-xa" 'anything)

;; Clojure
;; (setq inferior-lisp-program "~/Source/clojure/bin/clj")
(require 'clojure-mode)
(setq auto-mode-alist
      (cons '("\\.clj$" . clojure-mode)
      auto-mode-alist))

(add-hook 'clojure-mode-hook
	  '(lambda ()
	     (define-key clojure-mode-map "\C-c\C-e" 'lisp-eval-last-sexp)))

(setq swank-clojure-binary (concat home-path "/Source/clojure/bin/clj"))
(require 'swank-clojure-autoload)
(swank-clojure-config)

;; Set up the Common List environment
(setq inferior-lisp-program "/opt/local/bin/sbcl")
(require 'slime)
(slime-setup)

(add-to-list 'slime-lisp-implementations '(sbcl ("sbcl")))

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
