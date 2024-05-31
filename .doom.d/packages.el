;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

(package! evil-smartparens)
(package! evil-lisp-state)
(package! git-link)

(package! clj-refactor)
(package! multiple-cursors)
(package! inflections)

;; workaround for issue connecting to cider
(package! map :pin "bb50dba")

(package! gptel)
(package! pinentry)

(package! copilot
  :recipe (:host github :repo "zerolfx/copilot.el" :files ("*.el" "dist")))
