(defun dotspacemacs/layers ()
  "Layer configuration:
This function should only modify configuration layer settings."
  (setq-default
   dotspacemacs-distribution 'spacemacs
   dotspacemacs-configuration-layer-path '()
   dotspacemacs-configuration-layers
   '(html
     javascript
     ruby
     csv
     python
     sql
     yaml
     syntax-checking
     spell-checking
     themes-megapack
     (auto-completion :variables
                      auto-completion-complete-with-key-sequence "jk"
                      auto-completion-enable-sort-by-usage t
                      auto-completion-return-key-behavior nil
                      auto-completion-tab-key-behavior 'complete
                      auto-completion-enable-help-tooltip t)
     (org :variables
          org-enable-github-support t)
     (git :variables
          git-gutter-use-fringe t
          git-enable-github-support t)
     colors
     markdown
     ;; lsp
     (clojure :variables
              clojure-toplevel-inside-comment-form t
              clojure-enable-clj-refactor t
              clojure-enable-linters 'clj-kondo
              cider-overlays-use-font-lock t
              cider-repl-buffer-size-limit 100)
     evil-commentary
     ivy
     (rust :variables rust-backend 'racer))
   dotspacemacs-additional-packages '(solaire-mode
                                      flycheck-clojure
                                      evil-smartparens
                                      company-quickhelp
                                      edn)
   dotspacemacs-excluded-packages '()
   dotspacemacs-delete-orphan-packages t))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration."

  ;; cljstyle formatting
  (add-to-list 'load-path "~/play/cljstyle-mode.el")

  (setq-default
   dotspacemacs-editing-style 'vim
   dotspacemacs-verbose-loading nil
   dotspacemacs-startup-banner 'official
   dotspacemacs-elpa-https nil
   dotspacemacs-always-show-changelog t
   dotspacemacs-startup-lists '(recents projects)
   dotspacemacs-themes '(ujelly
                         sanityinc-tomorrow-eighties
                         sanityinc-tomorrow-bright
                         moe-dark)
   dotspacemacs-colorize-cursor-according-to-state t
   dotspacemacs-default-font '("Hack"
                               :size 18
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
   dotspacemacs-fullscreen-at-startup t
   dotspacemacs-fullscreen-use-non-native nil
   dotspacemacs-maximized-at-startup nil
   dotspacemacs-active-transparency 100
   dotspacemacs-inactive-transparency 70
   dotspacemacs-mode-line-unicode-symbols t
   dotspacemacs-mode-line-theme 'spacemacs
   dotspacemacs-smooth-scrolling t
   dotspacemacs-smartparens-strict-mode t
   dotspacemacs-persistent-server nil
   dotspacemacs-search-tools '("rg" "ag" "pt" "ack" "grep")
   dotspacemacs-line-numbers nil
   dotspacemacs-default-package-repository nil
   dotspacemacs-enable-server t
   dotspacemacs-persistent-server t
   dotspacemacs-frame-title-format "%I@%S"
   dotspacemacs-read-process-output-max (* 1024 1024)
   dotspacemacs-large-file-size 1))



(defun dotspacemacs/user-config ()
  (setq clojure-enable-fancify-symbols t
        cider-repl-use-pretty-printing t
        cider-repl-buffer-size-limit 16000
        cider-repl-history-file "~/.cider-repl-history"
        clojure-indent-style :align-arguments
        nrepl-use-ssh-fallback-for-remote-hosts t
        cider-auto-select-error-buffer nil
        cider-repl-user-clojure-font-lock nil
        cljr-clojure-test-declaration "[clojure.test :refer :all]"
        cljr-clojure-test-namespace-under-test-alias "impl"
        cljr-favor-prefix-notation nil
        cljr-auto-sort-ns t
        cider-auto-jump-to-error t
        cider-prompt-save-file-on-refresh nil
        cider-save-file-on-load nil
        company-quickhelp-delay 0
        projectile-mode-line "Projectile"
        cider-font-lock-dynamically '(macro core function var)
        cider-print-fn 'puget
        cider-print-options '(("namespace-maps" "true") ("seq-limit" 25))
        clojure-defun-style-default-indent nil
        ffap-machine-p-known 'reject
        tramp-default-method "ssh"
        powerline-default-separator 'arrow-fade
        vc-follow-symlinks t
        frame-resize-pixelwise t
        evil-ex-search-highlight-all nil
        projectile-enable-caching t
        rust-format-on-save t)

  (require 'cljstyle-mode)

  (spacemacs/toggle-evil-safe-lisp-structural-editing-on-register-hook-clojure-mode)

  (set-face-attribute 'flycheck-error nil :underline '(:color "#d32e00"))
  (set-face-attribute 'flycheck-warning nil :underline '(:color "#e3795c"))
  (set-face-attribute 'flycheck-info nil :underline '(:color "ForestGreen"))

  ;; Prevents clipboard paste when opening files with mouse
  (define-key spacemacs-buffer-mode-map [down-mouse-1] nil)

  (add-hook 'after-init-hook #'global-flycheck-mode)
  (add-hook 'after-change-major-mode-hook #'turn-on-solaire-mode)
  (add-hook 'cider-repl-mode-hook #'company-mode)
  (add-hook 'cider-mode-hook #'company-mode)
  (add-hook 'cider-repl-mode-hook #'cider-company-enable-fuzzy-completion)
  (add-hook 'cider-mode-hook #'cider-company-enable-fuzzy-completion)
  (add-hook 'cider-repl-mode-hook #'company-quickhelp-mode)
  (add-hook 'cider-mode-hook #'company-quickhelp-mode)

  (add-hook 'clojure-mode-hook 'cljstyle-mode)

  ;; Allow sending C-r in terminal
  (defun bb/send-C-r ()
    (interactive)
    (term-send-raw-string "\C-r"))

  (defun bb/setup-term-mode ()
    (evil-local-set-key 'insert (kbd "C-r") 'bb/send-C-r))

  (add-hook 'term-mode-hook 'bb/setup-term-mode)

  (with-eval-after-load 'clojure-mode
    (put-clojure-indent 'sdef 2)
    (put-clojure-indent 'for-all 1)
    (put-clojure-indent 'fdef 1)
    (put-clojure-indent 'success-deferred 0)
    (put-clojure-indent 'error-deferred 0)
    (put-clojure-indent ':import '(0 (0)))
    (put-clojure-indent ':require '(0 (0)))
    (put-clojure-indent 'it 2)
    (put-clojure-indent 'describe 1)))

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
 '(custom-safe-themes
   '("3a9f65e0004068ecf4cf31f4e68ba49af56993c20258f3a49e06638c825fbfb6" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" default))
 '(package-selected-packages
   '(lsp-pyright lsp-origami origami lsp-ivy lsp-mode dash-functional alchemist elixir-mode edn yasnippet-snippets yapfify yaml-mode ws-butler writeroom-mode winum which-key web-mode web-beautify volatile-highlights vi-tilde-fringe uuidgen use-package treemacs-projectile treemacs-persp treemacs-magit treemacs-evil toml-mode toc-org tagedit symon symbol-overlay string-inflection sql-indent spaceline-all-the-icons solaire-mode smeargle slim-mode seeing-is-believing scss-mode scala-mode sbt-mode sass-mode rvm ruby-tools ruby-test-mode ruby-refactor ruby-hash-syntax rubocopfmt rubocop rspec-mode robe restart-emacs rbenv rake rainbow-mode rainbow-identifiers rainbow-delimiters racer pytest pyenv-mode py-isort pug-mode prettier-js popwin pippel pipenv pip-requirements password-generator paradox ox-gfm orgit org-projectile org-present org-pomodoro org-mime org-download org-cliplink org-bullets org-brain open-junk-file noflet nodejs-repl mvn move-text moe-theme mmm-mode minitest meghanada maven-test-mode markdown-toc magit-svn magit-section magit-gitflow lsp-ui lsp-treemacs lsp-python-ms lsp-java lorem-ipsum livid-mode live-py-mode link-hint json-navigator json-mode js2-refactor js-doc indent-guide importmagic impatient-mode hybrid-mode hungry-delete hl-todo highlight-parentheses highlight-numbers highlight-indentation helm-xref helm-themes helm-swoop helm-pydoc helm-purpose helm-projectile helm-org-rifle helm-org helm-mode-manager helm-make helm-lsp helm-ls-git helm-gitignore helm-git-grep helm-flx helm-descbinds helm-css-scss helm-company helm-c-yasnippet helm-ag groovy-mode groovy-imports gradle-mode google-translate golden-ratio gnuplot gitignore-templates gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link gh-md fuzzy font-lock+ flyspell-correct-helm flycheck-rust flycheck-pos-tip flycheck-clj-kondo flx-ido fill-column-indicator fancy-battery eyebrowse expand-region evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-textobj-line evil-surround evil-smartparens evil-org evil-numbers evil-matchit evil-magit evil-lisp-state evil-lion evil-indent-plus evil-iedit-state evil-goggles evil-exchange evil-escape evil-ediff evil-commentary evil-cleverparens evil-args evil-anzu emmet-mode editorconfig dumb-jump dotenv-mode diminish devdocs define-word cython-mode csv-mode company-web company-tern company-statistics company-quickhelp company-lsp company-anaconda column-enforce-mode color-theme-sanityinc-tomorrow color-identifiers-mode clojure-snippets clj-refactor clean-aindent-mode cider-eval-sexp-fu chruby centered-cursor-mode cargo bundler blacken auto-yasnippet auto-highlight-symbol auto-dictionary aggressive-indent ace-link ace-jump-helm-line ac-ispell))
 '(safe-local-variable-values
   '((eval define-clojure-indent
           (:require 0)
           (:import
            '(0
              (0)))
           (defrecord
             '(1 nil
                 (:defn)))
           (forv 1)
           (for+ 1)
           (future-with 1)
           (start-unless 1)
           (stop-when 1)
           (do-at 1)
           (thrown\? 1)
           (thrown-with-msg\? 2))
     (javascript-backend . tern)
     (javascript-backend . lsp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
)
