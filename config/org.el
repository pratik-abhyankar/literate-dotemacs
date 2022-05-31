(setq-default  org-confirm-babel-evaluate nil)

(setq org-directory "~/org")               ;; My default 'org' directory
(bind-key "C-c a" 'org-agenda)             ;; Set 'C-c a' keybinding to open 'org-agenda'
(bind-key "C-c c" 'org-capture)            ;; Set 'C-c c' keybinding to execute 'org-capture'

(setq-default
 org-hide-leading-stars t                  ;; Hide leading stars
 org-hide-emphasis-markers t               ;; Hide the Org markup indicators
 org-startup-indented t                    ;; Auto indent the body under headlines
 org-cycle-separator-lines 1               ;; Leave a line between org items
 org-src-fontify-natively t)               ;; Enable source code highlighting

(setq-default
 org-log-into-drawer t                     ;; Use LOGBOOK drawer to take notes
 org-log-done t                            ;; Add 'closed' log when marked done
 org-special-ctrl-a/e t                    ;; C-{a,e} should behave differently on headings
 org-enforce-todo-dependencies t           ;; Enforce tasks should be completed in order
 org-enforce-todo-checkbox-dependencies t) ;; Enforce checklist items should be completed in order

(setq-default
 org-agenda-max-entries 10                 ;; Show at max 10 entries from my agenda
 org-agenda-dim-blocked-tasks nil          ;; highlights blocked tasks
 org-agenda-skip-scheduled-if-done t       ;; No need to honor schedule if the task is already done
 org-agenda-block-separator)               ;; Add a line to separate agenda items in dinstint sections

(setq org-todo-keywords
      '((sequence "ASSIGNED(a)"
                  "IMPLEMENTATION(i)"
                  "BLOCKED(b)"
                  "WAITING(w)"
                  "REVIEW(r)"
                  "RELEASE(e)"
                  "|"
                  "DELEGATED(l)"
                  "DONE(d)")))

(setq org-todo-keyword-faces
      '(("ASSIGNED" . "#dc322f")
        ("IMPLEMENTATION" . "#b58900")
        ("BLOCKED" . "#cb4b16")
        ("WAITING" . "#cb4b16")
        ("REVIEW" . "#2aa198")
        ("RELEASE" . "#268bd2")
        ("DELEGATED" . "#859900")
        ("DONE" . "#859900")))
