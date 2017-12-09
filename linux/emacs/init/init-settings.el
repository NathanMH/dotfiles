; Theme

(load-theme 'monokai t)

; Emacs Settings

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

; Misc Keybindings
(global-set-key (kbd "C-<right>") 'windmove-right)
(global-set-key (kbd "C-<left>") 'windmove-left)
(global-set-key (kbd "C-<up>") 'windmove-up)
(global-set-key (kbd "C-<down>") 'windmove-down)

(provide 'init-settings)
