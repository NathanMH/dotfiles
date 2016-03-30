; Mu4e

	(require 'mu4e)
	(require 'smtpmail)

; Settings

	(define-key evil-normal-state-map (kbd ",mail") 'mu4e)
	(define-key evil-normal-state-map (kbd",fetch") 'mu4e-get-mail-command)
	(setq
		mu4e-maildir		"/home/musicnate/.emacs.d/mail"
		mu4e-set-folder		"/home/musicnate/.emacs.d/mail/sent"
		mu4e-drafts-folder	"/home/musicnate/.emacs.d/mail/drafts"
		mu4e-trash-folder	"/home/musicnate/.emacs.d/mail/trash"
		mu4e-refile-folder	"/home/musicnate/.emacs.d/mail/archive"
		mu4e-get-mail-command   "offlineimap"
		mu4e-update-interval 600)
	(setq message-send-mail-function 	'smtpmail-send-it
		  smtpmail-smtp-server 			"hp93.hostpapa.com"
		  smtpmail-stream-type			'ssl
		  smtpmail-smtp-service			465)

	(setq mu4e-view-show-images t)
	(add-hook 'mu4e-view-mode-hook 'visual-line-mode)
	;(setq mu4e-view-prefer-html t)

	; Add hjkl keys to mu4e
	(eval-after-load 'mu4e
		'(progn
			; Start with normal mode
			(add-hook 'mu4e-view-mode-hook 'evil-normal-state)
			(add-hook 'mu4e-main-mode-hook 'evil-normal-state)
			(add-hook 'mu4e-headers-mode-hook 'evil-normal-state)

			; Use the standard bindings as a base
			(evil-make-overriding-map mu4e-view-mode-map 'normal t)
			(evil-make-overriding-map mu4e-main-mode-map 'normal t)
			(evil-make-overriding-map mu4e-headers-mode-map 'normal t)

			(evil-add-hjkl-bindings mu4e-view-mode-map 'normal
				"J" 'mu4e-headers-jump-to-maildir
				"j" 'evil-next-line
				"k" '<up>
				"C" 'mu4e-compose-new
				"o" 'mu4e-view-go-to-url
				"Q" 'mu4e-raw-view-quit-buffer)

			(evil-add-hjkl-bindings mu4e-headers-mode-map 'normal
				"J" 'mu4e-headers-jump-to-maildir
				"j" 'evil-next-line
				"k" 'mu4e-headers-prev
				"C" 'mu4e-compose-new
				"o" 'mu4e-view-message)

			(evil-add-hjkl-bindings mu4e-main-mode-map 'normal
				"J" 'mu4e-headers-jump-to-maildir
				"j" 'evil-next-line
				"RET" 'mu4e-view-message)))

	; Set the headers to show in inbox
	(setq mu4e-headers-fields
		  '((:human-date	.  12)
			(:from-or-to 	.  22)
			(:subject		.  nil)))

	(setq mu4e-maildir-shortcuts
		'(("/INBOX"				.				?j)
		("/INBOX.Programming"	.				?p)))

(provide 'user-init-mu4e)
