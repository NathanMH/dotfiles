; Evil Leader

(require 'evil-leader)
(require 'evil-nerd-commenter)

(global-evil-leader-mode)

(evil-leader/set-leader ",")
(evil-leader/set-key
    "q" 'kill-this-buffer
    "k" 'kill-buffer-and-window
	"d" 'delete-window
    "tt" 'save-buffer
    "m" 'right-char
    "s" 'avy-goto-char
    "l" 'avy-goto-line
    "b" 'helm-buffers-list
	"h" 'helm-find
	"|" 'split-window-right ; Split window vertically
	"-" 'split-window-below ; Split window horizontally
	"o" 'org-open-at-point
    )

; Fulscreen (still usefull for WSL)
(evil-leader/set-key
  "full" 'toggle-frame-fullscreen)

; Which-key
(evil-leader/set-key
  "which" 'which-key-show-major-mode)

; Term
(evil-leader/set-key
  "term" 'term-other-window)

; Nerd-Commenter
(evil-leader/set-key
  "comm" 'evilnc-comment-or-uncomment-lines)

; Org-Wiki
(evil-leader/set-key
  "wiki" 'org-wiki-index)

; Org
(define-key evil-normal-state-map (kbd ",org") (lambda () (interactive) (find-file "/home/musicnate/Documents/org/todo.org")))
(define-key evil-normal-state-map (kbd ",O") 'show-all)
(define-key evil-normal-state-map (kbd ",C") 'hide-sublevels)
(define-key evil-normal-state-map (kbd ",r") 'org-reload)
; For using tab in org, before "'require evil"
(setq evil-want-C-i-jump nil) 

; Mail
(evil-leader/set-key
    "email" 'mu4e
    "fetch" 'mu4e-get-mail-command)

(provide 'init-evil-leader)
