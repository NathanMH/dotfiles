; Rainbow-Delimiters

	(require 'paren)
    (require 'rainbow-delimiters)

; Settings

	; Rainbow Parens
		(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
	; Show Matching Parens
		(show-paren-mode 1)
		(setq show-paren-delay 0)

(provide 'init-rainbow-delimiters)
