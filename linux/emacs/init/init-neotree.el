;;; package --- Summary: Setup neotree
;;; Commentary:

(require 'neotree)

;;; Code:

(define-key evil-normal-state-map (kbd ",ne") (lambda () (interactive) (neotree-dir "/home/musicnate/Documents/")))
(define-key evil-normal-state-map (kbd "|") #'neotree-enter-vertical-split)
(add-hook 'neotree-mode-hook
    (lambda ()
	(define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter)
	(define-key evil-normal-state-local-map (kbd "SPC") 'neotree-enter)
	(define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
	(define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)))

(setq neo-theme (if (display-graphic-p) 'arrow 'classic))
(setq-default neo-show-hidden-files t)

(provide 'init-neotree)

;;; init-neotree ends here
