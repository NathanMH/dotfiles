;;; package --- Summary: Setup term
;;; Commentary:

;;; Code:

(setq explicit-shell-file-name "/bin/bash")

(defun term-other-window ()
  "Open term in another window"
  (interactive)
  (let ((buf (term "/bin/bash")))
	(switch-to-buffer (other-buffer buf))
	(switch-to-buffer-other-window buf)))

(provide 'init-term)

;;; init-term ends here
