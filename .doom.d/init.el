;;; init.el -*- lexical-binding: t; -*-
(doom! :input

       :completion
       (company +tng +childframe)
       (ivy +icons)

       :ui
       doom
       doom-dashboard
       doom-quit
       hl-todo
       (ligatures +extra)
       modeline
       ophints
       (popup +defaults)
       vc-gutter
       vi-tilde-fringe
       workspaces

       :editor
       (evil +everywhere)
       file-templates
       fold
       (format +onsave)
       multiple-cursors
       snippets

       :emacs
       (dired +icons)
       electric
       (ibuffer +icons)
       undo
       vc

       :term

       :checkers
       syntax

       :tools
       (eval +overlay)
       lookup
       magit
       make

       :os
       (:if IS-MAC macos)
       tty

       :lang
       clojure
       emacs-lisp
       markdown
       org
       sh
       yaml

       :email
       :app
       :config
       (default +bindings +smartparens))
