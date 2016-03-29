; Org Mode

	(require 'org)

; Settings

	; Open Agenda
		(define-key evil-normal-state-map (kbd ",org") (lambda () (interactive) (dired "/home/musicnate/Documents/org/")))
	; Open All Folds
		(define-key evil-normal-state-map (kbd ",O") 'show-all)
	; Close All Folds
		(define-key evil-normal-state-map (kbd ",C") 'hide-sublevels)
	; Reload Org File
		(define-key evil-normal-state-map (kbd ",r") 'org-reload)

	(setq org-agenda-files (quote ("/home/musicnate/Documents/org/Programming.org")))
	(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
	(setq org-return-follows-link t)

(provide 'user-init-org)
