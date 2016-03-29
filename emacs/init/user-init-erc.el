; ERC

	(require 'erc)

; Settings

	(erc-autojoin-mode t)
	(setq erc-autojoin-channels-alist
		'((".\\.freenode.net" "#emacs" "#vim" "##programming")))
	(erc-track-mode t)
	(setq erc-track-exclude-types '("JOIN" "NICK" "QUIT" "MODE" "324" "329" "332" "353" "477"))
	(setq erc-hide-list '("JOIN" "PART" "QUIT" "NICK"))
	(defun djcb-erc-start-or-switch ()
		"Connect to ERC, or switch to last active buffer"
		(interactive)
		(if (get-buffer "irc.freenode.net:6667") ;; ERC already active?
		(erc-track-switch-buffer 1) ;; Yes, switch to last active
		(when (y-or-n-p "Start ERC?") ;; No, ask to start erc
			(erc :server "irc.freenode.net" :port 6667 :nick "JuxtMusic" :full-name "MusicNate"))))

	; Start ERC
		(define-key evil-normal-state-map (kbd ",erc") 'djcb-erc-start-or-switch)

(provide 'user-init-erc)
