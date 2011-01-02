#!/bin/sh
mkdir -p elisp

# Org-mode
curl http://orgmode.org/org-7.01h.tar.gz | tar xzf - -C elisp

# BBDB
curl http://bbdb.sourceforge.net/bbdb-2.35.tar.gz | tar xzf - -C elisp

# W3M
curl http://cvs.namazu.org/emacs-w3m.tar.gz?view=tar | tar xzf - -C elisp

# Remember
curl http://download.gna.org/remember-el/remember-2.0.tar.gz | tar xzf - -C elisp

# Ruby
svn co http://svn.ruby-lang.org/repos/ruby/trunk/misc elisp/ruby-mode

# Clojure
git clone git://github.com/jochu/clojure-mode.git elisp/clojure-mode

# Swank-clojure
git clone git://github.com/jochu/swank-clojure.git elisp/swank-clojure

# Slime
git clone git://git.boinkor.net/slime.git elisp/slime

# Clojure and clojure-contrib
git clone git://github.com/richhickey/clojure.git clojure/clojure
git clone git://github.com/richhickey/clojure-contrib.git clojure/clojure-contrib

# Color theme
curl http://download.savannah.gnu.org/releases-noredirect/color-theme/color-theme-6.6.0.tar.gz | tar xzf - -C elisp

# Wanderlust
curl http://www.kanji.zinbun.kyoto-u.ac.jp/~tomo/lemi/dist/apel/apel-10.7.tar.gz | tar xzf - -C elisp
curl http://www.kanji.zinbun.kyoto-u.ac.jp/~tomo/lemi/dist/flim/flim-1.14/flim-1.14.9.tar.gz | tar xzf - -C elisp
curl http://www.kanji.zinbun.kyoto-u.ac.jp/~tomo/lemi/dist/semi/semi-1.14-for-flim-1.14/semi-1.14.6.tar.gz | tar xzf - -C elisp
curl http://www.jpl.org/elips/wl/snapshots/wanderlust-201008011102.tar.gz | tar xzf - -C elisp

# Magit
curl http://zagadka.vm.bytemark.co.uk/magit/magit-0.7.tar.gz | tar xzf - -C elisp

# Twitter
git clone git://git.busydoingnothing.co.uk/twitter.git elisp/twitter

# Rinari
git clone git://github.com/eschulte/rinari.git elisp/rinari
PWD = `pwd`
cd elisp/rinari
git submodule init
git submodule update
cd $PWD

# Yasnippet
curl http://yasnippet.googlecode.com/files/yasnippet-0.6.1c.tar.bz2 | tar xjf - -C elisp

# Jabber
#git clone git://emacs-jabber.git.sourceforge.net/gitroot/emacs-jabber/emacs-jabber elisp/emacs-jabber
#http://sourceforge.net/projects/emacs-jabber/files/emacs-jabber/0.8.0/emacs-jabber-0.8.0.tar.bz2/download

# Ruby Debug
# http://rubyforge.org/frs/download.php/46883/ruby-debug-extra-0.10.3.tar.gz

# Twittering mode
git clone git://github.com/hayamiz/twittering-mode.git elisp/twittering-mode

# GNUS
git clone http://git.gnus.org/gnus.git elisp/gnus

# G-Client
svn co http://emacspeak.googlecode.com/svn/trunk/lisp/g-client elisp/g-client

