; Evil

    (require 'evil-leader)
    (global-evil-leader-mode)
    (evil-leader/set-leader ",")
    (require 'evil)
    (evil-mode 1)

; Settings

    ; Key Bindings with leader
	(evil-leader/set-key
	    "b" 'ibuffer
	    "q" 'delete-window
	    "tt" 'save-buffer
	    "k" 'kill-buffer-and-window)

    ; Buffer Movement
	(define-key evil-normal-state-map (kbd "<tab> h") 'windmove-left)
	(define-key evil-normal-state-map (kbd "<tab> j") 'windmove-down)
	(define-key evil-normal-state-map (kbd "<tab> k") 'windmove-up)
	(define-key evil-normal-state-map (kbd "<tab> l") 'windmove-right)
    ; New Blank buffer
	(define-key evil-normal-state-map (kbd ",new") (lambda () (interactive) (switch-to-buffer (generate-new-buffer "buffer"))))
    ; Move to begin/end of line
	(define-key evil-normal-state-map (kbd "H") "^")
	(define-key evil-normal-state-map (kbd "L") "$")
    ; iBuffer use evil
	(evil-add-hjkl-bindings ibuffer-mode-map 'emacs)
    ; Toggle fullscreen
	(define-key evil-normal-state-map (kbd ",full") 'toggle-frame-fullscreen)
    ; Make tabs work...
	;(define-key evil-motion-state-map "\t" nil)
	;(global-set-key (kbd "TAB") 'self-insert-command)
    ; Close html tags easier
	(define-key evil-insert-state-map ",/" 'sgml-close-tag)

(provide 'user-init-evil)
