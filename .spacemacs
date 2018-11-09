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
   '(javascript
     ruby
     csv
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
     (git :variables
          git-gutter-use-fringe t
          git-enable-github-support t)
     colors
     markdown
     clojure
     evil-commentary
     ivy)
   dotspacemacs-additional-packages '(solaire-mode
                                      flycheck-clojure
                                      evil-smartparens)
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
   dotspacemacs-default-package-repository nil)

  )



(defun dotspacemacs/user-config ()
  (setq clojure-enable-fancify-symbols nil
        cider-cljs-lein-repl "(do (use 'figwheel-sidecar.repl-api) (start-figwheel!) (cljs-repl))"
        cider-repl-use-pretty-printing t
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
        projectile-mode-line "Projectile"
        cider-font-lock-dynamically '(macro core function var)
        cider-inject-dependencies-at-jack-in nil
        cider-pprint-fn 'puget
        clojure-defun-style-default-indent nil
        ffap-machine-p-known 'reject
        tramp-default-method "ssh"
        powerline-default-separator 'arrow-fade
        vc-follow-symlinks t
        admin-repl-directory "~/work/app/service/admin/admin-repl"
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

  ;; Allow sending C-r in terminal
  (defun bb/setup-term-mode ()
    (evil-local-set-key 'insert (kbd "C-r") 'bb/send-C-r))

  (defun bb/send-C-r ()
    (interactive)
    (term-send-raw-string "\C-r"))

  (add-hook 'term-mode-hook 'bb/setup-term-mode)

  (evil-define-key '(normal insert) clojure-mode-map
    ;; defun at point
    (kbd "C-;") 'cider-eval-defun-at-point
    (kbd "C-:") 'cider-pprint-eval-defun-at-point
    (kbd "C-'") 'cider-eval-defun-to-comment

    ;; sexp at point
    (kbd "s-;") 'cider-eval-sexp-at-point
    (kbd "s-:") 'cider-pprint-eval-sexp-at-point)

  (defun admin-repl ()
    (interactive)
    (let* ((env (ido-completing-read "Environment:" '("dev" "stage" "prod")))
           (stage (when (string= env "dev")
                    (let ((v (read-string "Staging environment: ")))
                      (when (not (string= v "")) v))))
           (default-directory admin-repl-directory)
           (base-command (concat default-directory "/repl.sh " env (when stage (format " -s %s" stage))))
           (start (shell-command-to-string (concat base-command " start")))
           (addr-str (replace-regexp-in-string
                      "\n\\'" ""
                      (shell-command-to-string (concat base-command " addr"))))
           (addr-port (split-string addr-str " "))
           (addr (car addr-port))
           (port (car (cdr addr-port)))
           (env-str (concat (format "admin-repl[env:%s]" env)
                            (when stage (format "[stage:%s]" stage))))
           (buffer (cider-repl-create '(:repl-type "clj"
                                                   :host addr
                                                   :port port
                                                   :project-dir admin-repl-directory
                                                   :session-name env-str))))
      (sesman-add-object 'CIDER env-str buffer t)
      (pop-to-buffer buffer)))


  (defun upstream-install ()
    (interactive)
    (shell-command-to-string "lein monolith each :upstream :parallel 6 install"))

  (defun link-project ()
    (interactive)
    (shell-command-to-string "lein monolith link :deep"))

  (with-eval-after-load 'clojure-mode
    (put-clojure-indent 'sdef 2)
    (put-clojure-indent 'for-all 1)
    (put-clojure-indent 'fdef 1)
    (put-clojure-indent 'success-deferred 0)
    (put-clojure-indent 'error-deferred 0)
    (put-clojure-indent ':import '(0 (0)))
    (put-clojure-indent ':require '(0 (0))))

  (fringe-mode '(1 . 1)))
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
 '(package-selected-packages
   (quote
    (polymode evil-mc evil-matchit dumb-jump doom-modeline counsel-projectile counsel eclim iedit smartparens projectile multiple-cursors avy magit git-commit ghub treepy graphql company pythonic ivy yasnippet org-plus-contrib evil hydra yasnippet-snippets yapfify yaml-mode ws-butler with-editor winum which-key wgrep web-beautify volatile-highlights vi-tilde-fringe uuidgen use-package undo-tree toml-mode toc-org symon swiper string-inflection sql-indent spaceline-all-the-icons solaire-mode smex smeargle shrink-path seeing-is-believing rvm ruby-tools ruby-test-mode ruby-refactor ruby-hash-syntax rubocop rspec-mode robe restart-emacs request rbenv rake rainbow-mode rainbow-identifiers rainbow-delimiters racer pyvenv pytest pyenv-mode py-isort prettier-js popwin pippel pipenv pip-requirements persp-mode password-generator paradox org-bullets open-junk-file noflet neotree mvn move-text moe-theme mmm-mode minitest meghanada maven-test-mode markdown-toc magit-svn magit-gitflow lorem-ipsum livid-mode live-py-mode link-hint json-navigator json-mode js2-refactor js-doc ivy-yasnippet ivy-xref ivy-purpose ivy-hydra indent-guide importmagic hungry-delete hl-todo highlight-parentheses highlight-numbers highlight-indentation helm-make groovy-mode groovy-imports gradle-mode goto-chg google-translate golden-ratio gitignore-templates gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link gh-md fuzzy font-lock+ flycheck-rust flycheck-pos-tip flycheck-clojure flx-ido fill-column-indicator fancy-battery eyebrowse expand-region evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-smartparens evil-numbers evil-magit evil-lisp-state evil-lion evil-indent-plus evil-iedit-state evil-goggles evil-exchange evil-escape evil-ediff evil-commentary evil-cleverparens evil-args evil-anzu ensime eldoc-eval editorconfig dotenv-mode diminish define-word cython-mode csv-mode company-tern company-statistics company-quickhelp company-emacs-eclim company-anaconda column-enforce-mode color-theme-sanityinc-tomorrow color-identifiers-mode clojure-snippets clojure-cheatsheet clj-refactor clean-aindent-mode cider-eval-sexp-fu chruby centered-cursor-mode cargo bundler auto-yasnippet auto-highlight-symbol aggressive-indent ace-window ace-link ac-ispell)))
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
                 (:defn))))
           (forv 1)
           (for+ 1))
     (eval define-clojure-indent
           (:require 0)
           (:import
            (quote
             (0
              (0))))
           (defrecord
             (quote
              (1 nil
                 (:defn))))
           (forv 1)
           (for+ 1)
           (do-at 1)
           (thrown\? 1)
           (thrown-with-msg\? 2))
     (eval define-clojure-indent
           (:require 0)
           (:import
            (quote
             (0
              (0))))
           (defrecord
             (quote
              (1 nil
                 (:defn))))
           (forv 1)
           (for+ 1)
           (future-with 1)
           (do-at 1)
           (thrown\? 1)
           (thrown-with-msg\? 2))
     (javascript-backend . tern)
     (javascript-backend . lsp)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
)
