;; Clojure
(setq swank-clojure-jar-path (concat dotemacs-path "/clojure/clojure/clojure.jar")
      swank-clojure-extra-classpaths (list
				      (concat vendor-path "/swank-clojure/src/main/clojure")
				      (concat dotemacs-path "/clojure/clojure-contrib/clojure-contrib.jar")))
(require 'clojure-mode)
(require 'swank-clojure)
