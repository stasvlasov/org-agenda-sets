`org-agenda-sets.el` provides macro to define sets of (org) files that can be used for `org-agenda-files` and  interactive function(s) that you can use to switch between sets and repopulate `org-agenda-files`. When files in sets are (asynchronously) scanned on your disk it saves it so you do not need to re-scan on next emacs session. It also provides basic sets operations (e.g., sets union, subtract, overlap).

Example configuration (with [straight.el](https://github.com/radian-software/straight.el) and [use-package](https://github.com/jwiegley/use-package)):

    (use-package org-agenda-sets
      :straight (:host github :repo "stasvlasov/org-agenda-sets")
      :demand t
      :after (org async dash f)
      :config
      ;;;; define sets
      (org-agenda-sets-define org
                              :dir ("~/org")
                              :files-filter (:ext ("org")))
      (org-agenda-sets-define work
                              :dir ("~/projects")
                              :files-filter (:ext ("org")))
      (org-agenda-sets-define notes
                              :sets (org work)
                              :files-filter (:ends ("notes.org")))
      (setq org-agenda-sets-default-set "org")
      ;;;; things you may want to keybind
      ;; org-agenda-sets-use
      ;; org-agenda-sets-add
      ;; org-agenda-sets-remove
      ;; org-agenda-sets-overlap
      ;; org-agenda-sets-reset
      ;;;; initiate default set 
      (org-agenda-sets-use org-agenda-sets-default-set))

