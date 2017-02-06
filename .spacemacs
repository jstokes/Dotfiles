;; -*- mode: dotspacemacs -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration."
  (setq-default
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (ie. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()
   ;; List of configuration layers to load. If it is the symbol `all' instead
   ;; of a list then all discovered layers will be installed.
   dotspacemacs-configuration-layers
   '(python
     lua
     ruby
     sql
     javascript
     yaml
     vimscript
     (auto-completion :variables
                      auto-completion-complete-with-key-sequence "jk"
                      auto-completion-enable-sort-by-usage t
                      auto-completion-return-key-behavior nil
                      auto-completion-tab-key-behavior 'complete
                      auto-completion-enable-help-tooltip t)
     (git :variables git-gutter-use-fringe t)
     (colors :variables colors-enable-nyan-cat-progress-bar t)
     markdown
     docker
     org
     osx
     clojure
     emacs-lisp
     evil-commentary
     themes-megapack)
   ;; A list of packages and/or extensions that will not be install and loaded.
   dotspacemacs-excluded-packages '()
   ;; If non-nil spacemacs will delete any orphan packages, i.e. packages that
   ;; are declared in a layer which is not a member of
   ;; the list `dotspacemacs-configuration-layers'
   dotspacemacs-delete-orphan-packages t))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration."
  (setq-default
   dotspacemacs-editing-style 'vim
   dotspacemacs-verbose-loading nil
   dotspacemacs-startup-banner 'official
   dotspacemacs-elpa-https nil
   dotspacemacs-always-show-changelog t
   dotspacemacs-startup-lists '(recents projects)
   dotspacemacs-themes '(sanityinc-tomorrow-bright
                         noctilux
                         zenburn
                         tango-dark
                         tango
                         whiteboard
                         monokai
                         solarized-light
                         solarized-dark
                         ujelly)
   dotspacemacs-colorize-cursor-according-to-state t
   dotspacemacs-default-font '("Hack"
                               :size 14
                               :weight normal
                               :width normal
                               :powerline-offset 2)
   dotspacemacs-leader-key "SPC"
   dotspacemacs-emacs-leader-key "M-m"
   dotspacemacs-major-mode-leader-key ","
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   dotspacemacs-command-key ":"
   dotspacemacs-enable-paste-micro-state t
   dotspacemacs-guide-key-delay 0.4
   dotspacemacs-loading-progress-bar t
   dotspacemacs-fullscreen-at-startup t
   dotspacemacs-fullscreen-use-non-native nil
   dotspacemacs-maximized-at-startup nil
   dotspacemacs-active-transparency 90
   dotspacemacs-inactive-transparency 90
   dotspacemacs-mode-line-unicode-symbols t
   dotspacemacs-smooth-scrolling t
   dotspacemacs-smartparens-strict-mode t
   dotspacemacs-persistent-server nil
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   dotspacemacs-default-package-repository nil)
   ;; User initialization goes here
  )

(defun dotspacemacs/user-config ()
  (setq clojure-enable-fancify-symbols t
        cider-auto-select-error-buffer nil
        cider-auto-jump-to-error t
        cider-font-lock-dynamically '(macro core function var)
        cider-pprint-fn 'fipp
        clojure-defun-style-default-indent nil
        ffap-machine-p-known 'reject)
  (fringe-mode '(1 . 1))
  "Configuration function.
 This function is called at the very end of Spacemacs initialization after
layers configuration.")

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
