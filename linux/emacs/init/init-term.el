
(setq explicit-shell-file-name "/bin/zsh")

(defun term-other-window ()
  "Open term in another window"
  (interactive)
  (let ((buf (term "/bin/zsh")))
	(switch-to-buffer (other-buffer buf))
	(switch-to-buffer-other-window buf)))

(provide 'init-term)
