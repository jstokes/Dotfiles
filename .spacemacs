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
   '(csv
     python
     sql
     yaml
     syntax-checking
     (auto-completion :variables
            auto-completion-complete-with-key-sequence "jk"
                      auto-completion-enable-sort-by-usage t
                      auto-completion-return-key-behavior nil
                      auto-completion-tab-key-behavior 'complete
                      auto-completion-enable-help-tooltip t)
     (git :variables git-gutter-use-fringe t)
     colors
     markdown
     clojure
     evil-commentary
     themes-megapack
     ivy)
   dotspacemacs-additional-packages '(solaire-mode
                                      flycheck-clojure
                                      ;; evil-smartparens
                                      magit-gh-pulls)
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
                         moe-dark)
   dotspacemacs-colorize-cursor-according-to-state t
   dotspacemacs-default-font '("Hack"
                               :size 13
                               :weight normal
                               :width normal
                               :powerline-scale 1.0)
   dotspacemacs-leader-key "SPC"
   dotspacemacs-emacs-leader-key "M-m"
   dotspacemacs-major-mode-leader-key ","
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   dotspacemacs-command-key ":"
   dotspacemacs-enable-paste-micro-state t
   dotspacemacs-guide-key-delay 0.4
   dotspacemacs-loading-progress-bar t
   dotspacemacs-fullscreen-at-startup nil
   dotspacemacs-fullscreen-use-non-native nil
   dotspacemacs-maximized-at-startup nil
   dotspacemacs-active-transparency 100
   dotspacemacs-inactive-transparency 70
   dotspacemacs-mode-line-unicode-symbols t
   dotspacemacs-smooth-scrolling t
   dotspacemacs-smartparens-strict-mode t
   dotspacemacs-persistent-server nil
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   dotspacemacs-line-numbers nil
   dotspacemacs-default-package-repository nil)
  ;;(push '("melpa-stable" . "stable.melpa.org/packages/") configuration-layer--elpa-archives)
  ;;(push '(cider . "melpa-stable") package-pinned-packages)
  ;;(push '(clj-refactor . "melpa-stable") package-pinned-packages)
  )

(defun dotspacemacs/user-config ()
  (setq clojure-enable-fancify-symbols nil
        cider-cljs-lein-repl "(do (use 'figwheel-sidecar.repl-api) (start-figwheel!) (cljs-repl))"
        clojure-indent-style :align-arguments
        nrepl-use-ssh-fallback-for-remote-hosts t
        cider-auto-select-error-buffer nil
        cljr-clojure-test-declaration "[clojure.test :refer :all]"
        cljr-clojure-test-namespace-under-test-alias "impl"
        cljr-favor-prefix-notation nil
        cider-auto-jump-to-error t
        cider-prompt-save-file-on-refresh nil
        cider-save-file-on-load nil
        projectile-mode-line "Projectile"
        cider-font-lock-dynamically '(macro core function var)
        cider-inject-dependencies-at-jack-in nil
        clojure-defun-style-default-indent nil
        ffap-machine-p-known 'reject
        tramp-default-method "ssh"
        powerline-default-separator 'arrow-fade
        column-enforce-column 100
        ;; actually fullscreen
        frame-resize-pixelwise t
        ;; try to stop freezing?
        evil-ex-search-highlight-all nil

        ;; enable fuzzy-ish searching with ivy
        ivy-re-builders-alist '((t . ivy--regex-fuzzy)))


  ;; Prevents clipboard paste when opening files with mouse
  (define-key spacemacs-buffer-mode-map [down-mouse-1] nil)


  (add-hook 'after-init-hook #'global-flycheck-mode)
  (add-hook 'after-change-major-mode-hook #'turn-on-solaire-mode)

  ;; (add-hook 'smartparens-enabled-hook #'evil-smartparens-mode)
  (add-hook 'clojure-mode-hook #'column-enforce-mode)
  (add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls)

  (evil-define-key '(normal insert) clojure-mode-map
    ;; defun at point
    (kbd "C-;") 'cider-eval-defun-at-point
    (kbd "C-:") 'cider-pprint-eval-defun-at-point
    (kbd "C-'") 'cider-eval-defun-to-comment

    ;; sexp at point
    (kbd "s-;") 'cider-eval-sexp-at-point
    (kbd "s-:") 'cider-pprint-eval-sexp-at-point)

  (defun indent-cond (indent-point state)
    (goto-char (elt state 1))
    (let ((pos -1)
          (base-col (current-column)))
      (forward-char 1)
      ;; `forward-sexp' will error if indent-point is after
      ;; the last sexp in the current sexp.
      (condition-case nil
          (while (and (<= (point) indent-point)
                      (not (eobp)))
            (clojure-forward-logical-sexp 1)
            (cl-incf pos))
        ;; If indent-point is _after_ the last sexp in the
        ;; current sexp, we detect that by catching the
        ;; `scan-error'. In that case, we should return the
        ;; indentation as if there were an extra sexp at point.
        (scan-error (cl-incf pos)))
      (+ base-col (if (evenp pos) 4 2))))

  (defun indent-cond-> (indent-point state)
    (goto-char (elt state 1))
    (let ((pos -1)
          (base-col (current-column)))
      (forward-char 1)
      ;; `forward-sexp' will error if indent-point is after
      ;; the last sexp in the current sexp.
      (condition-case nil
          (while (and (<= (point) indent-point)
                      (not (eobp)))
            (clojure-forward-logical-sexp 1)
            (cl-incf pos))
        ;; If indent-point is _after_ the last sexp in the
        ;; current sexp, we detect that by catching the
        ;; `scan-error'. In that case, we should return the
        ;; indentation as if there were an extra sexp at point.
        (scan-error (cl-incf pos)))
      (+ base-col (if (oddp pos) 4 2))))

  (with-eval-after-load 'clojure-mode
    (setq cider-pprint-fn "cider.nrepl.middleware.pprint/puget-pprint")
    (put-clojure-indent 'cond #'indent-cond)
    (put-clojure-indent 'condp #'indent-cond)
    (put-clojure-indent 'cond-> #'indent-cond->)
    (put-clojure-indent 'cond->> #'indent-cond->)
    (put-clojure-indent 'sdef 2)
    (put-clojure-indent 'for-all 1)
    (put-clojure-indent 'fdef 1)
    (put-clojure-indent 'success-deferred 0)
    (put-clojure-indent 'error-deferred 0)
    (put-clojure-indent ':import '(0 (0)))
    (put-clojure-indent ':require '(0 (0))))

  (fringe-mode '(1 . 1)))

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["#272822" "#F92672" "#A6E22E" "#E6DB74" "#66D9EF" "#FD5FF0" "#A1EFE4" "#F8F8F2"])
 '(ansi-term-color-vector
   [unspecified "#FFFFFF" "#d15120" "#5f9411" "#d2ad00" "#6b82a7" "#a66bab" "#6b82a7" "#505050"] t)
 '(background-color "#202020")
 '(background-mode dark)
 '(cljr-clojure-test-declaration "[clojure.test :refer :all]")
 '(cljr-clojure-test-namespace-under-test-alias "impl")
 '(clojure-indent-style :align-arguments)
 '(compilation-message-face (quote default))
 '(cursor-color "#cccccc")
 '(custom-safe-themes
   (quote
    ("06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" default)))
 '(default-input-method "ucs")
 '(evil-want-Y-yank-to-eol nil)
 '(fci-rule-character-color "#d9d9d9")
 '(fci-rule-color "#424242" t)
 '(foreground-color "#cccccc")
 '(highlight-changes-colors (quote ("#FD5FF0" "#AE81FF")))
 '(highlight-tail-colors
   (quote
    (("#3C3D37" . 0)
     ("#679A01" . 20)
     ("#4BBEAE" . 30)
     ("#1DB4D0" . 50)
     ("#9A8F21" . 60)
     ("#A75B00" . 70)
     ("#F309DF" . 85)
     ("#3C3D37" . 100))))
 '(hl-paren-background-colors (quote ("#2492db" "#95a5a6" nil)))
 '(hl-paren-colors (quote ("#ecf0f1" "#ecf0f1" "#c0392b")) t)
 '(magit-diff-use-overlays nil)
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(package-selected-packages
   (quote
    (doom-tomorrow-night-theme \(doom-one\ :location\ \(recipe\ :fetcher\ github\ :repo\ \"hlissner/emacs-doom-themes\"\)\)-theme csv-mode sayid parinfer sanityinc-tomorrow-night-theme-theme winum madhat2r-theme fuzzy evil-cleverparens solarized-theme evil-nerd-commenter define-word zonokai-theme zenburn-theme zen-and-art-theme yapfify yaml-mode ws-butler window-numbering which-key web-beautify volatile-highlights vimrc-mode vi-tilde-fringe uuidgen use-package underwater-theme ujelly-theme twilight-theme twilight-bright-theme twilight-anti-bright-theme tronesque-theme toxi-theme toc-org tao-theme tangotango-theme tango-plus-theme tango-2-theme sunny-day-theme sublime-themes subatomic256-theme subatomic-theme sql-indent spacemacs-theme spaceline powerline spacegray-theme soothe-theme soft-stone-theme soft-morning-theme soft-charcoal-theme smyx-theme smeargle seti-theme rvm ruby-tools ruby-test-mode rubocop rspec-mode robe reverse-theme reveal-in-osx-finder restart-emacs rbenv rake rainbow-mode rainbow-identifiers rainbow-delimiters railscasts-theme pyvenv pytest pyenv-mode py-isort purple-haze-theme professional-theme popwin planet-theme pip-requirements phoenix-dark-pink-theme phoenix-dark-mono-theme persp-mode pcre2el pbcopy pastels-on-dark-theme paradox osx-trash osx-dictionary orgit organic-green-theme org-projectile org-present org org-pomodoro alert log4e gntp org-plus-contrib org-download org-bullets open-junk-file omtose-phellack-theme oldlace-theme occidental-theme obsidian-theme noctilux-theme niflheim-theme neotree naquadah-theme mustang-theme move-text monokai-theme monochrome-theme molokai-theme moe-theme mmm-mode minitest minimal-theme material-theme markdown-toc markdown-mode majapahit-theme magit-gitflow macrostep lush-theme lua-mode lorem-ipsum livid-mode skewer-mode simple-httpd live-py-mode linum-relative link-hint light-soap-theme launchctl js2-refactor js2-mode js-doc jbeans-theme jazz-theme ir-black-theme inkpot-theme info+ indent-guide ido-vertical-mode hy-mode hungry-delete htmlize hl-todo highlight-parentheses highlight-numbers parent-mode highlight-indentation hide-comnt heroku-theme hemisu-theme help-fns+ helm-themes helm-swoop helm-pydoc helm-projectile helm-mode-manager helm-make projectile helm-gitignore request helm-flx helm-descbinds helm-company helm-c-yasnippet helm-ag hc-zenburn-theme gruvbox-theme gruber-darker-theme grandshell-theme gotham-theme google-translate golden-ratio gnuplot gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link gh-md gandalf-theme flycheck-pos-tip flycheck flx-ido flx flatui-theme flatland-theme firebelly-theme fill-column-indicator farmhouse-theme fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-mc evil-matchit evil-magit magit git-commit with-editor evil-lisp-state smartparens evil-indent-plus evil-iedit-state iedit evil-exchange evil-escape evil-ediff evil-commentary evil-args evil-anzu anzu evil goto-chg undo-tree espresso-theme elisp-slime-nav dumb-jump dracula-theme dockerfile-mode docker json-mode tablist magit-popup docker-tramp json-snatcher json-reformat django-theme diminish darktooth-theme autothemer darkokai-theme darkmine-theme darkburn-theme dakrone-theme dactyl-mode cython-mode cyberpunk-theme company-tern dash-functional tern company-statistics company-quickhelp pos-tip company-anaconda company column-enforce-mode color-theme-sanityinc-solarized color-identifiers-mode coffee-mode clues-theme clojure-snippets clj-refactor hydra inflections edn multiple-cursors paredit peg clean-aindent-mode cider-eval-sexp-fu eval-sexp-fu highlight cider seq spinner queue pkg-info clojure-mode epl chruby cherry-blossom-theme busybee-theme bundler inf-ruby bubbleberry-theme birds-of-paradise-plus-theme bind-map bind-key badwolf-theme auto-yasnippet yasnippet auto-highlight-symbol auto-compile packed apropospriate-theme anti-zenburn-theme anaconda-mode pythonic f dash s ample-zen-theme ample-theme alect-themes aggressive-indent afternoon-theme adaptive-wrap ace-window ace-link ace-jump-helm-line helm avy helm-core async ac-ispell auto-complete popup quelpa package-build color-theme-sanityinc-tomorrow)))
 '(pdf-view-midnight-colors (quote ("#DCDCCC" . "#383838")))
 '(pos-tip-background-color "#A6E22E")
 '(pos-tip-foreground-color "#272822")
 '(safe-local-variable-values
   (quote
    ((eval define-clojure-indent
           (:require 0)
           (:import
            (quote
             (0
              (0))))
           (defrecord
             (quote
              (1 nil
                 (:defn))))))))
 '(sml/active-background-color "#34495e")
 '(sml/active-foreground-color "#ecf0f1")
 '(sml/inactive-background-color "#dfe4ea")
 '(sml/inactive-foreground-color "#34495e")
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#d54e53")
     (40 . "#e78c45")
     (60 . "#e7c547")
     (80 . "#b9ca4a")
     (100 . "#70c0b1")
     (120 . "#7aa6da")
     (140 . "#c397d8")
     (160 . "#d54e53")
     (180 . "#e78c45")
     (200 . "#e7c547")
     (220 . "#b9ca4a")
     (240 . "#70c0b1")
     (260 . "#7aa6da")
     (280 . "#c397d8")
     (300 . "#d54e53")
     (320 . "#e78c45")
     (340 . "#e7c547")
     (360 . "#b9ca4a"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (unspecified "#272822" "#3C3D37" "#F70057" "#F92672" "#86C30D" "#A6E22E" "#BEB244" "#E6DB74" "#40CAE4" "#66D9EF" "#FB35EA" "#FD5FF0" "#74DBCD" "#A1EFE4" "#F8F8F2" "#F8F8F0")))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((((class color) (min-colors 89)) (:foreground "#ffffff" :background "#000000" :family "Hack" :foundry "nil" :slant normal :weight normal :height 130 :width normal))))
 '(whitespace-space ((t (:foreground "disabledControlTextColor")))))
(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["#272822" "#F92672" "#A6E22E" "#E6DB74" "#66D9EF" "#FD5FF0" "#A1EFE4" "#F8F8F2"])
 '(ansi-term-color-vector
   [unspecified "#FFFFFF" "#d15120" "#5f9411" "#d2ad00" "#6b82a7" "#a66bab" "#6b82a7" "#505050"] t)
 '(background-color "#202020")
 '(background-mode dark)
 '(cljr-clojure-test-declaration "[clojure.test :refer :all]")
 '(cljr-clojure-test-namespace-under-test-alias "impl")
 '(clojure-indent-style :align-arguments)
 '(compilation-message-face (quote default))
 '(cursor-color "#cccccc")
 '(custom-safe-themes
   (quote
    ("06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" default)))
 '(default-input-method "ucs")
 '(evil-want-Y-yank-to-eol nil)
 '(fci-rule-character-color "#d9d9d9")
 '(fci-rule-color "#424242")
 '(foreground-color "#cccccc")
 '(highlight-changes-colors (quote ("#FD5FF0" "#AE81FF")))
 '(highlight-tail-colors
   (quote
    (("#3C3D37" . 0)
     ("#679A01" . 20)
     ("#4BBEAE" . 30)
     ("#1DB4D0" . 50)
     ("#9A8F21" . 60)
     ("#A75B00" . 70)
     ("#F309DF" . 85)
     ("#3C3D37" . 100))))
 '(hl-paren-background-colors (quote ("#2492db" "#95a5a6" nil)))
 '(hl-paren-colors (quote ("#ecf0f1" "#ecf0f1" "#c0392b")))
 '(magit-commit-arguments nil)
 '(magit-diff-use-overlays nil)
 '(nrepl-message-colors
   (quote
    ("#CC9393" "#DFAF8F" "#F0DFAF" "#7F9F7F" "#BFEBBF" "#93E0E3" "#94BFF3" "#DC8CC3")))
 '(package-selected-packages
   (quote
    (sqlup-mode doom-tomorrow-night-theme \(doom-one\ :location\ \(recipe\ :fetcher\ github\ :repo\ \"hlissner/emacs-doom-themes\"\)\)-theme csv-mode sayid parinfer sanityinc-tomorrow-night-theme-theme winum madhat2r-theme fuzzy evil-cleverparens solarized-theme evil-nerd-commenter define-word zonokai-theme zenburn-theme zen-and-art-theme yapfify yaml-mode ws-butler window-numbering which-key web-beautify volatile-highlights vimrc-mode vi-tilde-fringe uuidgen use-package underwater-theme ujelly-theme twilight-theme twilight-bright-theme twilight-anti-bright-theme tronesque-theme toxi-theme toc-org tao-theme tangotango-theme tango-plus-theme tango-2-theme sunny-day-theme sublime-themes subatomic256-theme subatomic-theme sql-indent spacemacs-theme spaceline powerline spacegray-theme soothe-theme soft-stone-theme soft-morning-theme soft-charcoal-theme smyx-theme smeargle seti-theme rvm ruby-tools ruby-test-mode rubocop rspec-mode robe reverse-theme reveal-in-osx-finder restart-emacs rbenv rake rainbow-mode rainbow-identifiers rainbow-delimiters railscasts-theme pyvenv pytest pyenv-mode py-isort purple-haze-theme professional-theme popwin planet-theme pip-requirements phoenix-dark-pink-theme phoenix-dark-mono-theme persp-mode pcre2el pbcopy pastels-on-dark-theme paradox osx-trash osx-dictionary orgit organic-green-theme org-projectile org-present org org-pomodoro alert log4e gntp org-plus-contrib org-download org-bullets open-junk-file omtose-phellack-theme oldlace-theme occidental-theme obsidian-theme noctilux-theme niflheim-theme neotree naquadah-theme mustang-theme move-text monokai-theme monochrome-theme molokai-theme moe-theme mmm-mode minitest minimal-theme material-theme markdown-toc markdown-mode majapahit-theme magit-gitflow macrostep lush-theme lua-mode lorem-ipsum livid-mode skewer-mode simple-httpd live-py-mode linum-relative link-hint light-soap-theme launchctl js2-refactor js2-mode js-doc jbeans-theme jazz-theme ir-black-theme inkpot-theme info+ indent-guide ido-vertical-mode hy-mode hungry-delete htmlize hl-todo highlight-parentheses highlight-numbers parent-mode highlight-indentation hide-comnt heroku-theme hemisu-theme help-fns+ helm-themes helm-swoop helm-pydoc helm-projectile helm-mode-manager helm-make projectile helm-gitignore request helm-flx helm-descbinds helm-company helm-c-yasnippet helm-ag hc-zenburn-theme gruvbox-theme gruber-darker-theme grandshell-theme gotham-theme google-translate golden-ratio gnuplot gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link gh-md gandalf-theme flycheck-pos-tip flycheck flx-ido flx flatui-theme flatland-theme firebelly-theme fill-column-indicator farmhouse-theme fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-mc evil-matchit evil-magit magit git-commit with-editor evil-lisp-state smartparens evil-indent-plus evil-iedit-state iedit evil-exchange evil-escape evil-ediff evil-commentary evil-args evil-anzu anzu evil goto-chg undo-tree espresso-theme elisp-slime-nav dumb-jump dracula-theme dockerfile-mode docker json-mode tablist magit-popup docker-tramp json-snatcher json-reformat django-theme diminish darktooth-theme autothemer darkokai-theme darkmine-theme darkburn-theme dakrone-theme dactyl-mode cython-mode cyberpunk-theme company-tern dash-functional tern company-statistics company-quickhelp pos-tip company-anaconda company column-enforce-mode color-theme-sanityinc-solarized color-identifiers-mode coffee-mode clues-theme clojure-snippets clj-refactor hydra inflections edn multiple-cursors paredit peg clean-aindent-mode cider-eval-sexp-fu eval-sexp-fu highlight cider seq spinner queue pkg-info clojure-mode epl chruby cherry-blossom-theme busybee-theme bundler inf-ruby bubbleberry-theme birds-of-paradise-plus-theme bind-map bind-key badwolf-theme auto-yasnippet yasnippet auto-highlight-symbol auto-compile packed apropospriate-theme anti-zenburn-theme anaconda-mode pythonic f dash s ample-zen-theme ample-theme alect-themes aggressive-indent afternoon-theme adaptive-wrap ace-window ace-link ace-jump-helm-line helm avy helm-core async ac-ispell auto-complete popup quelpa package-build color-theme-sanityinc-tomorrow)))
 '(pdf-view-midnight-colors (quote ("#DCDCCC" . "#383838")))
 '(pos-tip-background-color "#A6E22E")
 '(pos-tip-foreground-color "#272822")
 '(safe-local-variable-values
   (quote
    ((eval define-clojure-indent
           (:require 0)
           (:import
            (quote
             (0
              (0))))
           (defrecord
             (quote
              (1 nil
                 (:defn))))))))
 '(sml/active-background-color "#34495e")
 '(sml/active-foreground-color "#ecf0f1")
 '(sml/inactive-background-color "#dfe4ea")
 '(sml/inactive-foreground-color "#34495e")
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#d54e53")
     (40 . "#e78c45")
     (60 . "#e7c547")
     (80 . "#b9ca4a")
     (100 . "#70c0b1")
     (120 . "#7aa6da")
     (140 . "#c397d8")
     (160 . "#d54e53")
     (180 . "#e78c45")
     (200 . "#e7c547")
     (220 . "#b9ca4a")
     (240 . "#70c0b1")
     (260 . "#7aa6da")
     (280 . "#c397d8")
     (300 . "#d54e53")
     (320 . "#e78c45")
     (340 . "#e7c547")
     (360 . "#b9ca4a"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (unspecified "#272822" "#3C3D37" "#F70057" "#F92672" "#86C30D" "#A6E22E" "#BEB244" "#E6DB74" "#40CAE4" "#66D9EF" "#FB35EA" "#FD5FF0" "#74DBCD" "#A1EFE4" "#F8F8F2" "#F8F8F0")))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((((class color) (min-colors 89)) (:foreground "#ffffff" :background "#000000" :family "Hack" :foundry "nil" :slant normal :weight normal :height 130 :width normal))))
 '(whitespace-space ((t (:foreground "disabledControlTextColor")))))
)
