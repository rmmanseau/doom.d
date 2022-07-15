;;; arista.el -*- lexical-binding: t; -*-

(use-package! gerrit
  :custom
  (gerrit-host "gerrit.corp.arista.io") ;; needed for REST API calls
  :config
  (progn
    ;; override function in gerrit.el
    (defun gerrit-query (query)
      "Perform a query QUERY and display it in a dashboard buffer."
      ;; TODO offer completion in interactive ...
      ;; TODO offer a list of candidates (history)
      (interactive "sEnter a query string: ")
      (let* ((buffer (format "gerrit:%s" query))
             (is-new (not (get-buffer buffer))))
        (switch-to-buffer buffer)
        (if is-new
            (progn
              (setq gerrit-dashboard-query-alist
                    ;; if car is nil gerrit.el will not display a section line
                    `((nil . ,(concat query " limit:50"))))
              (gerrit-dashboard-mode)))))

    ;; override function in gerrit-rest.el
    (defun gerrit-rest--get-gerrit-accounts ()
      "Return an alist of all active gerrit users."
      (interactive)
      (condition-case nil
          (let ((accounts (list))
                (batch (list))
                (n 0)
                (stop nil))
            (while (not stop)
              (progn
                (setq batch
                      (mapcar (lambda (account-info) (cons (cdr (assoc '_account_id account-info))
                                                           (cdr (assoc 'username account-info))))
                              ;; see https://gerrit-review.googlesource.com/Documentation/rest-api-accounts.html
                              ;; and https://gerrit-review.googlesource.com/Documentation/user-search-accounts.html#_search_operators
                              (gerrit-rest-sync "GET" nil (concat "/accounts/?q=is:active&o=DETAILS&S="
                                                                  (number-to-string n)))))
                (if (not (eq (length batch) 500))
                    (setq stop t))
                (setq n (+ n (length batch)))
                (setq accounts (append accounts batch))
                ))
            accounts)
        (error '())))
    ))

;; gerrit mode function defs
(defun my/gerrit-dashboard ()
  (interactive)
  (let ((gerrit-dashboard-query-alist
         '(("Assigned to me" . "assignee:self (-is:wip OR owner:self OR assignee:self) is:open -is:ignored")
           ("Work in progress" . "is:open owner:self is:wip")
           ("Outgoing reviews" . "is:open owner:self -is:wip -is:ignored")
           ("Incoming reviews" .  "is:open -owner:self -is:wip -is:ignored (reviewer:self OR assignee:self)")
           ("CCed On" . "is:open -is:ignored cc:self")
           ("Recently closed" . "is:closed -is:ignored (-is:wip OR owner:self) (owner:self OR reviewer:self OR assignee:self OR cc:self) limit:15"))
         )
        (gerrit-dashboard-buffer-name "*gerrit-dashboard*")
        )
    (gerrit-dashboard)))

(defun my/kill-gerrit-buffers ()
  (interactive)
  (kill-matching-buffers "^gerrit:\\w*" nil t))

(defun my/gerrit-project-dashboard ()
  (interactive)
  (gerrit-query (concat "project:" (gerrit-get-current-project) " is:open")))

(defun my/gerrit-query-owner (owner)
  (interactive "sEnter owner: ")
  (gerrit-query (concat "owner:" owner " is:open")))

(defun my/gerrit-query-project (project)
  (interactive "sEnter project: ")
  (gerrit-query (concat "project:" project " is:open")))

(map! (:after (gerrit)
      (:map gerrit-dashboard-mode-map
       :n "G" #'gerrit-dashboard--refresh
       :n "C-b" #'my/ivy-switch-buffer-git
       :n "e" #'gerrit-dashboard-edit-query
       :n "D" #'gerrit-download-transient
       :n "P" #'gerrit-upload-transient)
      (:leader :prefix "G"
       :desc "user dashboard" "g" #'my/gerrit-dashboard
       :desc "project dashboard" "p" #'my/gerrit-project-dashboard
       :desc "download" "d" #'gerrit-download-transient
       :desc "upload" "u" #'gerrit-upload-transient
       :desc "kill all gerrit buffers" "x" #'my/kill-gerrit-buffers
       :desc "switch buffer" "b" #'my/ivy-switch-buffer-git
       (:prefix ("q" . "query")
        :desc "query" "q" #'gerrit-query
        :desc "owner" "o" #'my/gerrit-query-owner
        :desc "project" "p" #'my/gerrit-query-project))))

;; magit / evil keybindings are kinda complicated. see:
;; SPC h p evil-collection
;; https://github.com/emacs-evil/evil-collection/tree/master/modes/magit
;;
;; to rebind keys in magit mode maps that conflict with vim motion,
;; be sure to include :n in front of the binding, ie
;; (map! :n "G" #'gerrit-dashboard)
(map! (:after (magit gerrit)
       :map magit-status-mode-map
       :n "C-d" #'gerrit-download-transient
       :n "C-u" #'gerrit-upload-transient))

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
