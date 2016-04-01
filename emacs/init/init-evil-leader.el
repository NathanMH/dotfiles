; Evil Leader

(require 'evil-leader)
(require 'evil-nerd-commenter)

(global-evil-leader-mode)

(evil-leader/set-leader ",")
(evil-leader/set-key
    "b" 'ibuffer
    "q" 'delete-window
    "tt" 'save-buffer
    "k" 'kill-buffer-and-window
    "m" 'right-char
    "," 'insert-comma)

(evil-leader/set-key
    "full" 'toggle-frame-fullscreen)

; Nerd-Commenter
(evil-leader/set-key
    "comm" 'evilnc-comment-or-uncomment-lines)

; Mail
(evil-leader/set-key
    "email" 'mu4e
    "fetch" 'mu4e-get-mail-command)

; Allows me to actually type a comma...TODO ability to do this in insert mode.
(defun insert-comma ()
    (interactive)
    (insert ","))

(provide 'init-evil-leader)
