;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Jeff Stokes"
      user-mail-address "jeffecu88@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))
;;
(setq doom-font (font-spec :family "Hack" :size 17))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-tomorrow-night)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type nil)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how

;; they are implemented.

(setq smartparens-strict-mode t)

(add-hook 'emacs-lisp-mode-hook #'smartparens-strict-mode)
(add-hook 'emacs-lisp-mode-hook #'evil-smartparens-mode)
(add-hook 'clojure-mode-hook #'smartparens-strict-mode)
(add-hook 'clojure-mode-hook #'evil-smartparens-mode)


;; Sets SPC k <key> to do a smartparens command and enter lisp state
;; (Borrowed from Spacemacs)
(use-package! evil-lisp-state
  :init (setq evil-lisp-state-global t)
  :config
  (evil-lisp-state-leader "SPC k")
  (setq evil-lisp-state-global t))

;; Fix issue where backspacing a close paren actually deletes the close paren without deleting the open paren
;; https://github.com/hlissner/doom-emacs/issues/4374
(map!
 :mode smartparens-mode
 :i
 "DEL" 'sp-backward-delete-char)

;; Fix issue where opening parens right before a word doesn't create a close paren
;; Caused by https://github.com/hlissner/doom-emacs/blob/develop/modules/config/default/config.el#L100-L104
;; Because Doom "does not support strict mode" https://discord.com/channels/406534637242810369/406554085794381833/799123610312114186
(after! smartparens
  (sp-pair "(" nil :unless '(:rem sp-point-before-word-p)))


(map!
 :map doom-leader-map
 ;; SPC SPC = M-x, like in spacemacs
 "SPC" 'counsel-M-x
 ;; SPC sc remove highlight
 "sc" 'evil-ex-nohighlight
 ;; Yank ring
 "yr" 'counsel-yank-pop)


;; Make recent files (that aren't buffers yet) appear in SPC b b
(setq ivy-use-virtual-buffers t)

;; Hide . and ..
(setq ivy-extra-directories nil)

(map!
 :map ivy-minibuffer-map
 ;; TAB and RET go into the selected directory or opens the selected file
 "RET" 'ivy-alt-done
 "TAB" 'ivy-alt-done
 ;; C-h goes up a dir (like spacemacs helm)
 "C-h" 'ivy-backward-delete-char)

;; Make leader key help menu show up quicker
(setq which-key-idle-delay 0.3)

;; Set cljstyle formatter
(set-formatter!
 'cljstyle "/usr/local/bin/cljstyle pipe"
 :modes '(clojure-mode clojurescript-mode))

(setq-hook! 'clojure-mode-hook +format-with-lsp nil)
(setq-hook! 'clojurescript-mode-hook +format-with-lsp nil)

;; fd to escape
(setq evil-escape-key-sequence "fd")

(after! (:or clojure-mode clojurescript-mode)
  (setq clojure-indent-style 'align-arguments)
  (setq cider-default-cljs-repl 'shadow)
  (setq cider-print-fn 'puget)
  (setq cider-shadow-cljs-parameters "server -d djblue/portal:0.9.0"))

;; don't close my repl window plz
(after! cider
  (set-popup-rule! "^*cider-repl" :quit nil :ttl nil))

;; Doom modeline
(setq doom-modeline-height 1)
(set-face-attribute 'mode-line nil :family "Hack" :height 150)

(after! doom-modeline
  (setq doom-modeline-persp-name t))

(after! lsp-mode
  (setq lsp-enable-file-watchers nil
        lsp-enable-indentation nil
        lsp-ui-doc-show-with-cursor t
        lsp-ui-sideline-show-code-actions nil
        lsp-modeline-code-actions-enable nil
        lsp-completion-enable-additional-text-edit nil
        lsp-ui-doc-enable t
        lsp-lens-enable t
        lsp-enable-symbol-highlighting nil
        lsp-signature-render-documentation t))


(setq undo-limit 80000000           ;; Undo limit 80mb
      evil-want-fine-undo t         ;; More graunular undo for evil
      auto-save-default t           ;; Nobody likes to loose work, I certainly don't
      truncate-string-ellipsis "…") ;; Unicode ellispis are nicer than "...", and also save /precious/ space

;; Show battery when battery powered
(when (not (equal "Battery status not available" (battery)))
  (display-battery-mode 1))


;; Split to the right and below
(setq evil-vsplit-window-right t
      evil-split-window-below t)

;; Preview buffers when opening splits
(setq +ivy-buffer-preview t)

;; clj-refactor keybindings
(map!
 :map clojure-mode-map
 (:localleader
  :n  "r"      nil
  :n  "r ?"    #'cljr-describe-refactoring
  :n  "r a d"  #'cljr-add-declaration
  :n  "r a i"  #'cljr-add-import-to-ns
  :n  "r a m"  #'cljr-add-missing-libspec
  :n  "r a p"  #'cljr-add-project-dependency
  :n  "r a r"  #'cljr-add-require-to-ns
  :n  "r a s"  #'cljr-add-stubs
  :n  "r a u"  #'cljr-add-use-to-ns
  :n  "r c c"  #'cljr-cycle-coll
  :n  "r c i"  #'cljr-cycle-if
  :n  "r c n"  #'cljr-clean-ns
  :n  "r c p"  #'cljr-cycle-privacy
  :n  "r d k"  #'cljr-destructure-keys
  :n  "r e f"  #'cljr-extract-function
  :n  "r e c"  #'cljr-extract-constant
  :n  "r e l"  #'cljr-expand-let
  :n  "r f u"  #'cljr-find-usages
  :n  "r f e"  #'cljr-create-fn-from-example
  :n  "r h d"  #'cljr-hotload-dependency
  :n  "r i l"  #'cljr-introduce-let
  :n  "r i s"  #'cljr-inline-symbol
  :n  "r m f"  #'cljr-move-form
  :n  "r m l"  #'cljr-move-to-let
  :n  "r p c"  #'cljr-project-clean
  :n  "r p f"  #'cljr-promote-function
  :n  "r r d"  #'cljr-remove-debug-fns
  :n  "r r f"  #'cljr-rename-file-or-dir
  :n  "r r l"  #'cljr-remove-let
  :n  "r r r"  #'cljr-remove-unused-requires
  :n  "r r s"  #'cljr-rename-symbol
  :n  "r r u"  #'cljr-replace-use
  :n  "r s n"  #'cljr-sort-ns
  :n  "r s p"  #'cljr-sort-project-dependencies
  :n  "r s r"  #'cljr-stop-referring
  :n  "r s c"  #'cljr-show-changelog
  :n  "r t f"  #'cljr-thread-first-all
  :n  "r t h"  #'cljr-thread
  :n  "r t l"  #'cljr-thread-last-all
  :n  "r u a"  #'cljr-unwind-all
  :n  "r u p"  #'cljr-update-project-dependencies
  :n  "r u w"  #'cljr-unwind))


(map!
 :after lsp-mode
 (:leader
  :n  "l"      nil
  :n  "l l"    #'lsp-ui-imenu
  :n  "l a i"  #'lsp-clojure-add-import-to-namespace
  :n  "l a m"  #'lsp-clojure-add-missing-libspec
  :n  "l c c"  #'lsp-clojure-cycle-coll
  :n  "l c n"  #'lsp-clojure-clean-ns
  :n  "l c p"  #'lsp-clojure-cycle-privacy
  :n  "l e f"  #'lsp-clojure-extract-function
  :n  "l e l"  #'lsp-clojure-expand-let
  :n  "l f u"  #'lsp-find-references
  :n  "l i l"  #'lsp-clojure-introduce-let
  :n  "l i s"  #'lsp-clojure-inline-symbol
  :n  "l m l"  #'lsp-clojure-move-to-let
  :n  "l r l"  #'lsp-clojure-expand-let
  :n  "l r s"  #'lsp-rename
  :n  "l s n"  #'lsp-clojure-clean-ns
  :n  "l t f"  #'lsp-clojure-thread-first-all
  :n  "l t h"  #'lsp-clojure-thread-first
  :n  "l t l"  #'lsp-clojure-thread-last-all
  :n  "l u a"  #'lsp-clojure-unwind-all
  :n  "l u w"  #'lsp-clojure-unwind-thread))


;; Admin repl!
(defun admin-repl (cluster)
  (interactive "sCluster: ")
  (message (format "Connecting to %s." cluster))
  (shell-command (format "~/work/app/service/admin/admin-repl/repl.sh %s start" cluster))
  (cl-destructuring-bind (host port) (split-string
                                      (shell-command-to-string
                                       (format "~/work/app/service/admin/admin-repl/repl.sh %s addr" cluster)))
    (cider-connect-clj (list :host host
                             :port port
                             :project-dir "~/work/app/service/admin/admin-repl"))))
