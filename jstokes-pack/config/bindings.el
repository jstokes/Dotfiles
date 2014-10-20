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

(global-evil-leader-mode)

(evil-leader/set-leader "SPC")

(evil-leader/set-key-for-mode 'cider-mode
  "t" 'cider-reset-test-run-tests
  "e" 'cider-repl-reset
  "n" 'cider-repl-set-ns
  "k" 'cider-load-current-buffer
  "c" 'cider-eval-defun-at-point
  "d" 'cider-doc-map
  "r" 'cider-switch-to-repl-buffer)

(evil-leader/set-key
  "f" 'ido-find-file
  "b" 'ido-switch-buffer
  "d" 'describe-function)
