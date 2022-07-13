;;; arista.el -*- lexical-binding: t; -*-

(use-package! gerrit
  :custom
  (gerrit-host "gerrit.corp.arista.io")) ;; is needed for REST API calls

;; magit / evil keybindings are kinda complicated. see:
;; SPC h p evil-collection
;; ~/.emacs.d/.local/straight/repos/evil-collection/modes/magit/
;; https://github.com/emacs-evil/evil-collection/tree/master/modes/magit
;;
;; to rebind keys in magit mode maps that conflict with vim motion,
;; be sure to include :n in front of the binding, ie
;; (map! :n "G" #'gerrit-dashboard)
(map! (:after (magit gerrit)
       :map magit-status-mode-map
       :n "G" #'gerrit-dashboard
       :n "D" #'gerrit-download-transient
       :n "P" #'gerrit-upload-transient))

;; figure out mappings for gerrit upload / download transient states
;; maybe append them to the magit transients for push and pull?
;; look into #'transient-insert-suffix
;;
;; ~/.emacs.d/modules/tools/magit/config.el
;; (transient-append-suffix 'magit-fetch "-p"
;;   '("-t" "Fetch all tags" ("-t" "--tags")))

;; user server related stuff
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
