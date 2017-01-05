; Twittering

	(require 'twittering-mode)

; Settings

	(setq twittering-use-master-password t)
	(setq twittering-icon-mode t)
	(setq twittering-convert-fix-size 48)

	(define-key evil-normal-state-map (kbd ",twit") 'twit)
    (define-key twittering-mode-map (kbd ",b") 'ibuffer)

(provide 'init-twittering)
