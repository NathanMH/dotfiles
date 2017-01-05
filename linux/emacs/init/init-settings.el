; Theme

	(load-theme 'monokai t)

; Emacs Settings

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
	; Line numbers
		(global-linum-mode t)
	; Highlight current line
		(global-hl-line-mode t)
	; Linewrap
		(set-default 'truncate-lines t)
	; Auto complete quotes and parens
		(electric-pair-mode 1)
	; No startup splash/screen
		(setq inhibit-startup-buffer-menu t)
		(setq inhibit-startup-screen t)
	; Remove toolbar/menu/tooltips
		(tool-bar-mode 0)
		(menu-bar-mode 0)
		(tooltip-mode 0)
	; Scratch buffer
		(setq initial-scratch-message nil)
	; Make tab work/folding...
                (define-globalized-minor-mode global-hs-minor-mode hs-minor-mode hs-minor-mode)
                (global-hs-minor-mode 1)
;		(global-set-key (kbd "TAB") 'self-insert-command)
;		(global-set-key (kbd "<backspace>") 'backspace-soft-tab)
;		(global-set-key (kbd "<deletechar>") 'delete-forward-soft-tab)
;		(setq-default tab-width 4)
(provide 'init-settings)
