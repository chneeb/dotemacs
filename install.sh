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

