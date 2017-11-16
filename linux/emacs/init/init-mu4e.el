; Mu4e

	(require 'mu4e)
	(require 'smtpmail)

; Settings

	;(define-key evil-normal-state-map (kbd ",mail") 'mu4e)
	(define-key evil-normal-state-map (kbd",fetch") 'mu4e-get-mail-command)
    (setq mu4e-get-mail-command "fetchmail"); Command to get mail
	(setq mu4e-view-show-images t)
	(add-hook 'mu4e-view-mode-hook 'visual-line-mode) ; No text wrap
	;(setq mu4e-view-prefer-html t)

	; Set the headers to show in inbox
	(setq mu4e-headers-fields
		  '((:human-date	.  12)
			(:from-or-to 	.  22)
			(:subject		.  nil)))

    (setq mu4e-maildir "/home/musicnate/Documents/mail/")    ; Set top-level mail directory

    ; Default account
	(setq
		mu4e-sent-folder            	"/INBOX.Sent"
		mu4e-drafts-folder	            "/INBOX.Drafts"
		mu4e-trash-folder           	"/INBOX.Trash"
		mu4e-refile-folder	            "/INBOX.Archive"
        user-mail-address               "nathan@musicnate.ca"
        smtpmail-smtp-server 			"hp93.hostpapa.com"
        smtpmail-stream-type			'ssl
        smtpmail-smtp-service			465
		mu4e-update-interval 600
        message-send-mail-function 	'smtpmail-send-mail)

	(setq mu4e-maildir-shortcuts
		'(("/INBOX"				.				?j)
            ("/INBOX.Family"    .               ?p)
            ("/INBOX.Lara"      .               ?l)))

	; Custom keybindings for mu4e (vim bindings)
    (with-eval-after-load "mu4e"
        (evil-make-overriding-map mu4e-main-mode-map 'normal t)
        (evil-define-key 'normal mu4e-main-mode-map
            "," nil     ; Ensure my leader works properly
            "j" nil     ; Remove jump to maildir binding
            "gt" 'mu4e~headers-jump-to-maildir)    

        (evil-make-overriding-map mu4e-headers-mode-map 'normal t)
        (evil-define-key 'normal mu4e-headers-mode-map
            "j" 'evil-next-line
            "k" 'evil-previous-line
            "r" 'mu4e-compose-reply
            "c" 'mu4e-compose-new
            "ESC" nil
            "d" 'mu4e-headers-mark-for-trash
            "o" 'mu4e-headers-view-message
            "gt" 'mu4e~headers-jump-to-maildir)

        (evil-make-overriding-map mu4e-view-mode-map 'normal t)
        (evil-define-key 'normal mu4e-view-mode-map
            "j" 'evil-next-line
            "k" 'evil-previous-line
            "r" 'mu4e-compose-reply
            "c" 'mu4e-compose-new
            "ESC" nil
            "d" 'mu4e-view-mark-for-trash
            "n" 'mu4e-view-headers-next
            "p" 'mu4e-view-headers-prev
            "gt" 'mu4e~headers-jump-to-maildir)
        
        (evil-make-overriding-map mu4e-compose-mode-map 'normal t)
        (evil-define-key 'normal mu4e-compose-mode-map
            "c" nil))

(evil-set-initial-state 'mu4e-mode 'normal)
(evil-set-initial-state 'mu4e-main-mode 'normal)
(evil-set-initial-state 'mu4e-headers-mode 'normal)
(evil-set-initial-state 'mu4e-view-mode 'normal)
(evil-set-initial-state 'mu4e-compose-mode 'normal)

(provide 'init-mu4e)
