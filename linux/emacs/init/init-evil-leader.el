; Evil Leader

(require 'evil-leader)
(require 'evil-nerd-commenter)

(global-evil-leader-mode)

(evil-leader/set-leader ",")
(evil-leader/set-key
    "q" 'kill-this-buffer
    "tt" 'save-buffer
    "k" 'kill-buffer-and-window
    "m" 'right-char
    "s" 'avy-goto-char
    "l" 'avy-goto-line
    "b" 'ace-jump-buffer
	"h" 'helm-find
    )

(evil-leader/set-key
    "full" 'toggle-frame-fullscreen)

; Nerd-Commenter
(evil-leader/set-key
    "comm" 'evilnc-comment-or-uncomment-lines)

; Org-Wiki
(evil-leader/set-key
    "wiki" 'org-wiki-index
 )

; Mail
(evil-leader/set-key
    "email" 'mu4e
    "fetch" 'mu4e-get-mail-command)

; Org
(define-key evil-normal-state-map (kbd ",org") (lambda () (interactive) (find-file "/home/musicnate/Documents/org/todo.org")))
(define-key evil-normal-state-map (kbd ",O") 'show-all)
(define-key evil-normal-state-map (kbd ",C") 'hide-sublevels)
(define-key evil-normal-state-map (kbd ",r") 'org-reload)
; For using tab in org, before "'require evil"
(setq evil-want-C-i-jump nil) 

(provide 'init-evil-leader)
