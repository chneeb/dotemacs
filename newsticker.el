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
