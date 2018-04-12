;;; package --- Summary: Setup annotations
;;; Commentary:
; General Settings

;;; Code:

(load-theme 'monokai t)

(require 'soft-tab)

(add-to-list 'default-frame-alist '(fullscreen . fullboth))

; Allow y or n instead of yes or no
(defalias 'yes-or-no-p 'y-or-n-p)

; Auto complete ignore case
(setq read-file-name-completion-ignore-case t)
(setq read-buffer-name-completion-ignore-case t)

; Stop making shitty backup files
(setq make-backup-files nil)

; Shutup about compile saving
(setq compilation-ask-about-save nil)
(setq compilation-save-buffers-predicate '(lambda () nil))

; Highlight current line
(global-hl-line-mode t)

; Linewrap
(set-default 'truncate-lines t)

; Line numbers
(global-linum-mode t)

; Auto complete quotes and parens
(setq electric-pair-preserve-balance t)
(setq electric-pair-delete-adjacent-pairs t)
(setq blink-matching-paren t)
(electric-pair-mode 1)

; No startup splash/screen
(setq inhibit-startup-buffer-menu t)
(setq inhibit-startup-screen t)

; Visual Bell
(setq visible-bell t)

; Pretty Symbols (put a list of symbols here later...)
(global-prettify-symbols-mode t)
(setq prettify-symbols-unprettify-at-point t) ; Only in Emacs 25

; Remove toolbar/menu/tooltips add tabs
(tool-bar-mode 0)
(menu-bar-mode 0)
(tooltip-mode 0)

; Scratch buffer
(setq initial-scratch-message nil)

; Make tab work/folding...
;(define-globalized-minor-mode global-hs-minor-mode hs-minor-mode hs-minor-mode)
;(global-hs-minor-mode 1)
(global-set-key (kbd "TAB") 'self-insert-command)
(global-set-key (kbd "<backspace>") 'backspace-soft-tab)
(global-set-key (kbd "<deletechar>") 'delete-forward-soft-tab)
(setq-default tab-width 4)

; Sort Dired buffers
(defun mydired-sort ()
 (save-excursion
  (let (buffer-read-only)
   (forward-line 2)
   (sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max)))
  (set-buffer-modified-p nil)))

(defadvice dired-readin
 (after dired-after-updating-hook first () activate)
 (mydired-sort))

; Misc Keybindings
(global-set-key (kbd "C-M-l") 'windmove-right)
(global-set-key (kbd "C-M-h") 'windmove-left)
(global-set-key (kbd "C-M-k") 'windmove-up)
(global-set-key (kbd "C-M-j") 'windmove-down)

(defun dired-do-command (command)
  "Run COMMAND on marked files. Any files not already open will be opened.
After this command has been run, any buffers it's modified will remain
open and unsaved."
  (interactive
   (list
    (let ((print-level nil)
          (minibuffer-history-position 0)
          (minibuffer-history-sexp-flag (1+ (minibuffer-depth))))
      (unwind-protect
          (read-from-minibuffer
           "Command: " (prin1-to-string (nth 0 command-history))
           read-expression-map t
           (cons 'command-history 0))

        ;; If command was added to command-history as a
        ;; string, get rid of that.  We want only
        ;; evaluable expressions there.
        (if (stringp (car command-history))
            (setq command-history (cdr command-history)))))))
  (dolist (filename (dired-get-marked-files))
    (with-current-buffer (find-file-noselect filename)
      (if (symbolp command)
          (call-interactively command)
        (eval command)))))

(provide 'init-settings)

;;; init-settings ends here
