; Evil Leader

(require 'evil-leader)
(require 'evil-nerd-commenter)

(global-evil-leader-mode)

(evil-leader/set-leader ",")
(evil-leader/set-key
    "b" 'ibuffer
    "q" 'delete-window
    "tt" 'save-buffer
    "k" 'kill-buffer-and-window)

(evil-leader/set-key
    "full" 'toggle-frame-fullscreen)

; Nerd-Commenter
(evil-leader/set-key
    "comm" 'evilnc-comment-or-uncomment-lines)

(provide 'init-evil-leader)
