; Parens

	(require 'paren)

; Settings

	; Rainbow Parens
		(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
	; Show Matching Parens
		(show-paren-mode 1)
		(setq show-paren-delay 0)

(provide 'user-init-parens)
