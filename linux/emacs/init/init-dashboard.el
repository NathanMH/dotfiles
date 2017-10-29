; Dashboard

(require 'page-break-lines)
(require 'dashboard)


; Settings

(dashboard-setup-startup-hook)

(use-package dashboard
 :config
 (dashboard-setup-startup-hook))


(provide 'init-dashboard)
