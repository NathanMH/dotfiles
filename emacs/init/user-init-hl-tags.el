; Highlight Tags in and HTMl XML 

(require 'hl-tags-mode)

; Settings

(add-hook 'sgml-mode-hook (lambda () (hl-tags-mode 1)))
(add-hook 'nxml-mode-hook (lambda () (hl-tags-mode 1)))

(provide 'user-init-hl-tags)