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
(setq doom-font (font-spec :family "Hack" :size 18))

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

;; Tab to indent lines
(map!
 :mode clojure-mode
 :mode emacs-lisp-mode
 :ni
 "<tab>" 'lisp-indent-line)

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
 "sc" 'evil-ex-nohighlight)


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

;; fd to escape
(setq evil-escape-key-sequence "fd")

(after! (:or clojure-mode clojurescript-mode)
  (setq clojure-indent-style 'align-arguments)
  (setq cider-default-cljs-repl 'shadow)
  (setq cider-print-fn 'puget)
  (setq cider-shadow-cljs-parameters "server -d djblue/portal:0.9.0"))

;; Doom modeline
(setq doom-modeline-height 1)
(set-face-attribute 'mode-line nil :family "Noto Sans" :height 150)
