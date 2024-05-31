;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "Jeff Stokes"
      user-mail-address "jeffecu88@gmail.com")

;; ## Top Level Keybindings

(map!
 :map doom-leader-map
 "SPC" 'counsel-M-x
 "sc" 'evil-ex-nohighlight
 "yr" 'counsel-yank-pop
 "ai" 'gptel)


;; ## UI Settings

(setq
 auto-save-default t
 display-line-numbers-type nil
 doom-font (font-spec :family "Iosevka" :size 20)
 doom-modeline-height 1
 doom-theme 'doom-ir-black
 evil-escape-key-sequence "fd"
 evil-split-window-below t
 evil-vsplit-window-right t
 evil-want-fine-undo t
 +ivy-buffer-preview t
 ivy-extra-directories nil
 ivy-use-virtual-buffers t
 org-directory "~/org/"
 truncate-string-ellipsis "…"
 undo-limit 80000000
 vimish-fold-marks '(";; {" . ";; }")
 which-key-idle-delay 0.3
 git-link-default-branch "main")

(after! doom-modeline
  (setq doom-modeline-persp-name t))

(set-face-attribute 'mode-line nil :family "Iosevka" :height 140)

(when (not (equal "Battery status not available" (battery)))
  (display-battery-mode 1))

(map!
 :map ivy-minibuffer-map
 ;; TAB and RET go into the selected directory or opens the selected file
 "RET" 'ivy-alt-done
 "TAB" 'ivy-alt-done
 ;; C-h goes up a dir (like spacemacs helm)
 "C-h" 'ivy-backward-delete-char)


;; ##Pinentry

(use-package! pinentry
  :init (setq epa-pinentry-mode `loopback)
  (pinentry-start))

;; ## Smartparens

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


;; ## Cider/Clojure

(setq-default cider-use-overlays t)

(after! (:or clojure-mode clojurescript-mode)
  (setq clojure-indent-style 'align-arguments)
  (setq cider-default-cljs-repl 'shadow)
  (setq cider-print-fn 'fipp)
  (setq cider-print-options '(("print-length" 50)))
  (setq cider-print-quota 524288)
  (setq cider-shadow-cljs-parameters "server -d djblue/portal:0.9.0")
  (setq cider-enrich-classpath t)
  (setq cider-use-overlays t)
  (setq cider-save-file-on-load nil)
  (setq cider-overlays-use-font-lock t))

(after! cider
  (set-popup-rule! "^*cider-repl" :quit nil :ttl nil))

(set-formatter!
  'cljstyle '("cljstyle" "pipe")
  :modes '(clojure-mode clojurescript-mode clojurec-mode))

(setq +format-on-save-enabled-modes
      '(clojure-mode
        clojurescript-mode
        clojurec-mode
        (not markdown-mode)))

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


;; ## Magic/AI

(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
              ("TAB" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion-by-word)
              ("C-<tab>" . 'copilot-accept-completion-by-word)))

(setq copilot-indent-offset-warning-disable t)

(use-package! gptel)

(defun my/company-complete-or-next ()
  "Complete the current selection or select the next item."
  (interactive)
  (cond ((not company-candidates) ;; no popup is active
         (company-manual-begin))
        ((eq company-selection nil) ;; no completion is selected
         (company-select-first)
         (company-complete-selection))
        (t ;; a completion is selected
         (company-complete-selection))))

(with-eval-after-load 'company
  (define-key company-active-map (kbd "C-SPC") 'my/company-complete-or-next))

(with-eval-after-load 'evil
  (define-key evil-insert-state-map (kbd "C-SPC") 'my/company-complete-or-next))

;; Load any host specific settings if they exist
(load! "host.el" nil t)
