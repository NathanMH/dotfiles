;;; package --- Summary: Setup evil-addons
;;; Commentary:

(require 'evil-surround)
(require 'evil-terminal-cursor-changer)
(require 'evil-org)
;(require 'evil-mu4e)

;;; Code:

(setq evil-visual-state-cursor '("purple" hbar))
(setq evil-normal-state-cursor '("green" box))
(setq evil-insert-state-cursor '("red" bar))

(add-hook 'org-mode-hook 'evil-org-mode)
(evil-org-set-key-theme '(navigation insert textobjects additional calendar))

; Surround
(global-evil-surround-mode 1)
(add-to-list 'evil-surround-pairs-alist '(?* . ("*" . "*")))

(provide 'init-evil-addons)

;;; init-evil-addons ends here
