; iBuffer

	(require 'ibuffer)

; Settings
	
	(add-hook 'ibuffer-mode-hook (lambda () (ibuffer-auto-mode 1)))
	(setq ibuffer-expert t)
       
       ; Create default groups for ibuffer
	(setq ibuffer-saved-filter-groups
		(quote (("default"
			("Programming"
				(or
					(mode . c-mode)
					(mode . c++-mode)
					(mode . ruby-mode)
					(mode . python-mode)
					(mode . emacs-lisp-mode)))
			("HTML"
				(mode . html-mode))
			("CSS"
				(mode . css-mode))
			("Git"
				(name . "magit"))
			("Org" ;; Org related buffers
				(or
					(mode . org-mode)
					(name . "org")))
			("Calendar"				   
				(mode . cfw:calendar-mode))
			("Mail"
				(or
					(mode . message-mode)
					(mode . mail-mode)
					(mode . mu4e-main-mode)
					(mode . mu4e-headers-mode)
					(mode . mu4e-view-mode)
					(mode . mu4e-compose-mode)))
			("Chat"
				(mode . erc-mode)
				(mode . twitter-mode))
			("Twitter"
				(mode . twittering-mode))
			("Emacs"
				(name . "^\\*"))))))


	; Add default groups to ibuffer
		(add-hook 'ibuffer-mode-hook (lambda () (ibuffer-switch-to-saved-filter-groups "default")))

	; Collapse groups by default
		(setq mp/ibuffer-collapsed-groups (list "Default"))
		(defadvice ibuffer (after collapse-helm)
			(dolist (group mp/ibuffer-collapsed-groups)
				(progn
					(goto-char 1)
					(when (search-forward (concat "[ " group " ]") (point-max) t)
						(progn
							(move-beginning-of-line-nil)
							(ibuffer-toggle-filter-group))))))
	; Hide empty filter groups
	(setq ibuffer-show-empty-filter-groups nil)
	(ad-activate 'ibuffer)

(provide 'user-init-ibuffer)

