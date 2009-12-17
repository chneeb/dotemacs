#!/bin/sh
mkdir -p elisp

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

# Color theme
curl http://download.savannah.gnu.org/releases-noredirect/color-theme/color-theme-6.6.0.tar.gz | tar xzf - -C elisp

# Wanderlust
curl http://www.kanji.zinbun.kyoto-u.ac.jp/~tomo/lemi/dist/apel/apel-10.7.tar.gz | tar xzf - -C elisp
curl http://www.kanji.zinbun.kyoto-u.ac.jp/~tomo/lemi/dist/flim/flim-1.14/flim-1.14.9.tar.gz | tar xzf - -C elisp
curl http://www.kanji.zinbun.kyoto-u.ac.jp/~tomo/lemi/dist/semi/semi-1.14-for-flim-1.14/semi-1.14.6.tar.gz | tar xzf - -C elisp
curl http://www.jpl.org/elips/wl/snapshots/wanderlust-200908202051.tar.gz | tar xzf - -C elisp

# Magit
curl http://zagadka.vm.bytemark.co.uk/magit/magit-0.7.tar.gz | tar xzf - -C elisp

# Twitter
git clone git://git.busydoingnothing.co.uk/twitter.git elisp/twitter

# Rinari
git clone git://github.com/eschulte/rinari.git elisp/rinari
cd elisp/rinari
git submodule init
git submodule update

