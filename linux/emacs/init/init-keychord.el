; Key-chord

(require 'key-chord)

; Settings

(key-chord-mode 1)
(key-chord-define evil-insert-state-map (kbd "nn") 'evil-normal-state)
(key-chord-define evil-insert-state-map (kbd ",m") 'right-char)
(setq key-chord-delay 0.1)

(provide 'init-keychord)
