; NeoTree

(require 'neotree)

; Settings

(define-key evil-normal-state-map (kbd ",ne") (lambda () (interactive) (neotree-dir "/home/musicnate/Documents/")))
(define-key evil-normal-state-map (kbd ",s") #'neotree-enter-vertical-split)
(add-hook 'neotree-mode-hook
    (lambda ()
	(define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter)
	(define-key evil-normal-state-local-map (kbd "SPC") 'neotree-enter)
	(define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
	(define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)))

(provide 'init-neotree)
