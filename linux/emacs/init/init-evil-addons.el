; Evil Addons

(require 'evil-surround)
(require 'evil-terminal-cursor-changer)
(require 'evil-org)
;(require 'evil-mu4e)

; Settings

(global-evil-surround-mode 1)
(setq evil-visual-state-cursor '("purple" hbar))
(setq evil-normal-state-cursor '("green" box))
(setq evil-insert-state-cursor '("red" bar))

(provide 'init-evil-addons)
