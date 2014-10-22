;; Place your bindings here.

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

(set-face-attribute 'default nil :height 150)

(defun save-all ()
  (interactive)
  (save-some-buffers t))

(add-hook 'focus-out-hook 'save-all)

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
  "d"  'cider-doc-map
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
  "q" 'evil-delete-buffer
  "w" 'save-buffer
  "e" 'eval-defun
  "f" 'find-file-in-project
  "b" 'ido-switch-buffer
  "d" 'describe-function
  "-" 'text-scale-decrease
  "+" 'text-scale-increase)

(global-evil-leader-mode)

(require 'flx-ido)
(ido-mode t)
(ido-everywhere t)
(flx-ido-mode t)
;; disable ido faces to see flx highlights.
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)
