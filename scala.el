;; Scala
(require 'scala-mode)
(add-to-list 'auto-mode-alist '("\\.scala$" . scala-mode))
(setq yas/my-directory (concat vendor-path "/scala-mode/contrib/yasnippet/snippets"))
(yas/load-directory yas/my-directory)
(add-hook 'scala-mode-hook
          '(lambda ()
             (yas/minor-mode-on)))
