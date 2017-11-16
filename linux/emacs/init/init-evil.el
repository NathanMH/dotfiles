; Evil

(require 'evil)
(evil-mode 1)

; Settings

; Buffer Movement
(define-key evil-normal-state-map (kbd "C-h") 'windmove-left)
(define-key evil-normal-state-map (kbd "C-j") 'windmove-down)
(define-key evil-normal-state-map (kbd "C-k") 'windmove-up)
(define-key evil-normal-state-map (kbd "C-l") 'windmove-right)

; Move to begin/end of line
(define-key evil-normal-state-map (kbd "H") "^")
(define-key evil-normal-state-map (kbd "L") "$")

; iBuffer use evil
(evil-add-hjkl-bindings ibuffer-mode-map 'emacs)

; Close html tags easier (CAN'T BE DONE IN EVIL-LEADER)
;(define-key evil-insert-state-map ",/" 'sgml-close-tag)

; Use org-mode tab function
(evil-define-key 'normal org-mode-map (kbd "<tab>") #'org-cycle)

(provide 'init-evil)
