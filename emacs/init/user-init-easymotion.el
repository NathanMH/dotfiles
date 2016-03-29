; EasyMotion

	(require 'ace-jump-mode)
	(autoload 'ace-jump-char-mode "ace-jump-char-mode" "Emacs quick move minor mode" t)
	(setq ace-jump-mode-case-fold t)

; Settings

	(define-key evil-normal-state-map (kbd "s") 'ace-jump-char-mode)

(provide 'user-init-easymotion)
