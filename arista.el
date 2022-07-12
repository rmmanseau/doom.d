;;; arista.el -*- lexical-binding: t; -*-

(use-package! gerrit
  :custom
  (gerrit-host "gerrit.corp.arista.io")  ;; is needed for REST API calls
  :config
  ;; (progn
    ;; (add-hook 'magit-status-sections-hook #'gerrit-magit-insert-status t)
    ;; (global-set-key (kbd "C-x i") 'gerrit-upload-transient)
    ;; (global-set-key (kbd "C-x o") 'gerrit-download))
  )

(when (locate-library "Arastra")
  (load-library "Arastra")
  (map! :leader
        (:prefix ("a" . "arista")
         "a" #'a4-add
         "e" #'a4-edit
         "r" #'a4-revert
         "b" #'a4-bug
         "s" #'a4-gid2
         "d" #'a4-gid2-defs
         "g" #'a-grok))
  (add-to-list 'tramp-methods
               '("a4ssh"
                 (tramp-login-program "a4 ssh")
                 (tramp-login-args
                  (("-l" "%u")
                   ("-p" "%p")
                   ("%c")
                   ("-e" "none")
                   ("%h")))
                 (tramp-async-args
                  (("-q")))
                 (tramp-remote-shell "/bin/sh")
                 (tramp-remote-shell-login
                  ("-l"))
                 (tramp-remote-shell-args
                  ("-c"))
                 (tramp-gw-args
                  (("-o" "GlobalKnownHostsFile=/dev/null")
                   ("-o" "UserKnownHostsFile=/dev/null")
                   ("-o" "StrictHostKeyChecking=no")))
                 (tramp-default-port 22)
                 (tramp-connection-timeout 10))))
