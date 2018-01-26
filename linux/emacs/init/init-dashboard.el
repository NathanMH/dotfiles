;;; package --- Summary: Setup dashboard
;;; Commentary:

(require 'page-break-lines)
(require 'dashboard)

;;; Code:

(dashboard-setup-startup-hook)

(setq dashboard-items '((recents . 8)
                        (bookmarks . 5)
                        (projects . 5)
                        (agenda . 5)
                        ))

(provide 'init-dashboard)
;;; init-dashboard ends here
