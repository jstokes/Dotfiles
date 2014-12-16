;; ;; Place your bindings here.

(live-set-default-font "M+ 1mn 15")

; https://gist.github.com/txus/5420665
; map jk to ESC
(define-key evil-insert-state-map "j" #'cofi/maybe-exit)
(evil-define-command cofi/maybe-exit ()
  :repeat change
  (interactive)
  (let ((modified (buffer-modified-p)))
    (insert "j")
    (let ((evt (read-event (format "Insert %c to exit insert state" ?k)
                           nil 0.5)))
      (cond
       ((null evt) (message ""))
       ((and (integerp evt) (char-equal evt ?k))
        (delete-char -1)
        (set-buffer-modified-p modified)
        (push 'escape unread-command-events))
       (t (setq unread-command-events (append unread-command-events
                                              (list evt))))))))

(define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
(define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
(define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
(define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)

(define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
(define-key evil-visual-state-map (kbd "C-u") 'evil-scroll-up)


(define-key evil-normal-state-map (kbd "+") 'er/expand-region)
(define-key evil-visual-state-map (kbd "x") 'er/expand-region)
(define-key evil-visual-state-map (kbd "X") 'er/contract-region)

(define-key evil-normal-state-map (kbd ";") 'evil-ex)
(define-key evil-visual-state-map (kbd ";") 'evil-ex)


(defun save-all ()
  (interactive)
  (save-some-buffers t))

(add-hook 'focus-out-hook 'save-all)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(defun my-evil-modeline-change (default-color)
  "changes the modeline color when the evil mode changes"
  (let ((color (cond ((evil-insert-state-p) '("#002233" . "#ffffff"))
                     ((evil-visual-state-p) '("#330022" . "#ffffff"))
                     ((evil-normal-state-p) default-color)
                     (t '("#440000" . "#ffffff")))))
    (set-face-background 'mode-line (car color))
    (set-face-foreground 'mode-line (cdr color))))

(lexical-let ((default-color (cons (face-background 'mode-line)
                                   (face-foreground 'mode-line))))
  (add-hook 'post-command-hook (lambda () (my-evil-modeline-change default-color))))

(setq evil-emacs-state-cursor '("red" box))
(setq evil-normal-state-cursor '("green" box))
(setq evil-visual-state-cursor '("orange" box))
(setq evil-insert-state-cursor '("red" bar))
(setq evil-replace-state-cursor '("red" bar))
(setq evil-operator-state-cursor '("red" hollow))

(setq-local interprogram-cut-function nil)
(setq-local interprogram-paste-function nil)

(setq
 cider-repl-pop-to-buffer-on-connect nil
 nrepl-hide-special-buffers t
 cider-prompt-save-file-on-load nil
 evil-want-C-u-scroll t
 undo-tree-auto-save-history t
 undo-tree-history-directory-alist (quote (("." . "~/.emacs.d/undo/"))))

(defun cider-reset-test-run-tests ()
  (interactive)
  (cider-load-current-buffer)
  (cider-test-run-tests))

(evil-leader/set-leader "SPC")

(evil-leader/set-key-for-mode 'clojure-mode
  "a"  'clojure-jump-between-tests-and-code
  "d"  'ac-nrepl-popup-doc
  "t"  'clojure-test-run-test
  "T"  'clojure-test-run-test
  "e"  'cider-eval-defun-at-point
  "n"  'cider-repl-set-ns
  "k"  'cider-load-current-buffer
  "cq" 'cider-quit
  "cc" 'cider-connect
  "cj" 'cider-jack-in
  "cq" 'cider-quit
  "re" 'cider-switch-to-repl-buffer
  "rs" 'cljr-sort-ns
  "rr" 'cljr-add-require-to-ns
  "ru" 'cljr-add-use-to-ns)

(evil-leader/set-key
  "s" 'evil-window-split
  "v" 'evil-window-vsplit
  "q" 'delete-window
  "db" 'evil-delete-buffer
  "df" 'describe-function
  "dw" 'delete-window
  "w" 'save-buffer
  "e" 'eval-defun
  "lb" 'eval-buffer
  "f" 'find-file-in-project
  "b" 'ido-switch-buffer
  "n" 'evil-next-buffer
  "N" 'evil-prev-buffer
  "B" 'ido-switch-buffer-other-window
  "g" 'magit-status
  "G" 'magit-blame-mode
  "-" 'text-scale-decrease
  "+" 'text-scale-increase)

(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
    In Delete Selection mode, if the mark is active, just deactivate it;
    then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))

(ac-set-trigger-key "TAB")

(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

(global-evil-leader-mode)

(find-file "~/.live-packs/jstokes-pack/config/bindings.el")
