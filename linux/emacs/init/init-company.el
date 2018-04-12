;;; package --- Summary: Setup company
;;; Commentary:

;;; Code:

(add-hook 'after-init-hook 'global-company-mode)

(defun my/python-mode-hook ()
  (add-to-list 'company-backends 'company-jedi))

(add-hook 'python-mode-hook 'my/python-mode-hook)

(provide 'init-company)

;;; init-company ends here
