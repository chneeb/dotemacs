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
(global-hl-line-mode t)
(delete-selection-mode t)
(setq make-backup-files nil)
(prefer-coding-system 'utf-8)
(server-start)
(setq-default indent-tabs-mode nil)
(global-set-key "\r" 'newline-and-indent) ; auto indent
(global-set-key (kbd "<s-return>") 'ns-toggle-fullscreen) ; Mac OS X fullscreen mode
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

;; package.el, ELPA and Marmalade
(load (expand-file-name (concat dotemacs-path "/elpa/package.el")))
(mapc '(lambda (item) (add-to-list 'package-archives item))
            '(("elpa" . "http://tromey.com/elpa/")
              ("technomancy" . "http://repo.technomancy.us/emacs/")
              ("marmalade" . "http://marmalade-repo.org/packages/")))
(package-initialize)

;; Color theme stuff
;;(require 'color-theme)
;;(color-theme-zenburn)

;; (set-frame-parameter (selected-frame) 'alpha '(<active> [<inactive>]))
;;(set-frame-parameter (selected-frame) 'alpha '(90 90))
;;(add-to-list 'default-frame-alist '(alpha 90 90))

;; Turn off anti-aliasing
;;(setq mac-allow-anti-aliasing nil)

(set-face-attribute 'default nil :height 140)

;;(load-file (concat dotemacs-path "/bbdb.el"))

;; ido mode
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t)
(setq ido-create-new-buffer 'always)

(load-file (concat dotemacs-path "/org-mode.el"))

;;(load-file (concat dotemacs-path "/w3m.el"))

(load-file (concat dotemacs-path "/ruby.el"))

;;(load-file (concat dotemacs-path "/clojure.el"))

(require 'slime)

(eval-after-load "slime" (slime-setup '(slime-repl)))
(slime-setup)

;; OpenSSL
(setq ssl-program-name "openssl")
(setq ssl-program-arguments '("s_client" "-quiet" "-host" host "-port" service))
(setq ssl-certificate-verification-policy 0)

;;(load-file (concat dotemacs-path "/twitter.el"))

;; Magit
(require 'magit)

;;(load-file (concat dotemacs-path "/jabber.el"))

;;text-mode
;; (add-hook 'text-mode-hook
;;   (lambda() 
;;     (set-fill-column 78)                    ; lines are 78 chars long ...
;;     (auto-fill-mode t)                      ; ... and wrapped around 
;;     (set-input-method "latin-1-prefix")))    ; make " + e => ë etc.

;; Yasnippet
(require 'yasnippet)
(yas/initialize)
;;(yas/load-directory (concat vendor-path "/yasnippet-0.6.1c/snippets"))

;; Deft - http://jblevins.org/projects/deft/
(require 'deft)
(setq deft-extension "org")
(setq deft-directory "~/Dropbox/Deft")
(setq deft-text-mode 'org-mode)

;; Simple RTM (Remember the Milk)
(autoload 'simple-rtm-mode "simple-rtm" "Interactive mode for Remember The Milk" t)

;; Textile
;;(require 'textile-mode)
;;(add-to-list 'auto-mode-alist '("\\.textile\\'" . textile-mode))

;;(load-file (concat dotemacs-path "/scala.el"))

;; Ant
;;(require 'ant)

;;(load-file (concat dotemacs-path "/newsticker.el"))

;; gnus
;;(load-file (concat dotemacs-path "/gnus.el"))

;; wanderlust
;;(load-file (concat dotemacs-path "/wl.el"))

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

;; Some keyboard shortcuts
(global-set-key (kbd "<f1>") '(lambda ()
                                (interactive)
                                (switch-to-buffer "*scratch*")))
(global-set-key (kbd "<f3>") '(lambda ()
                                (interactive)
                                (find-file (concat dotemacs-path "/init.el"))))
(global-set-key (kbd "<f4>") 'deft)
(global-set-key (kbd "<f5>") 'compile)
(global-set-key (kbd "<f6>") 'magit-status)

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
;; - http://nakkaya.com/2009/12/01/adding-inferior-lisp-support-for-clojure-mode/
;; - https://github.com/aemoncannon/ensime
;; - Rainbow cat - http://nyan-mode.buildsomethingamazing.com/
;; - http://emacs-fu.blogspot.com/2011/05/toward-balanced-and-colorful-delimiters.html
;; - R and Emacs  - http://languageagnostic.blogspot.com/2011/09/r-and-emacs.html
;; - Scheme in Emacs - http://languageagnostic.blogspot.com/2011/05/mit-scheme-in-emacs.html
;; - Deft Note Taking - http://jblevins.org/projects/deft/
;; - Collab Editor - http://www.emacswiki.org/emacs/Rudel
;; - MiniMap - http://www.emacswiki.org/emacs/MiniMap
;; - AceJump
;; - EmacsRocks Screencasts - http://emacsrocks.com
;; - Variable Width Fonts - http://xahlee.blogspot.com/2010/07/how-to-quickly-switch-fonts-in-emacs.html
;; - Sauron - http://emacs-fu.blogspot.com/2011/12/sauron-keeping-eye-on-whats-going-on.html
;; - Quack for Scheme - http://www.neilvandyke.org/quack/
