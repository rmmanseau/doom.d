;;; arista.el -*- lexical-binding: t; -*-

(use-package! gerrit
  :custom
  (gerrit-host "gerrit.corp.arista.io") ;; needed for REST API calls
  :config
  (progn

    ;;
    ;; fix error in gerrit-query, see https://github.com/thisch/gerrit.el/issues/40
    ;;
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

    ;;
    ;; override with updated function at https://github.com/thisch/gerrit.el/issues/8
    ;;
    (defun gerrit-rest--get-gerrit-accounts ()
      "Return an alist of all active gerrit users."
      (interactive)
      (condition-case nil
          (let ((continue t)
                (start-idx 0)
                (accounts '()))
            (while continue
              (let ((response
                     ;; see https://gerrit-review.googlesource.com/Documentation/rest-api-accounts.html
                     ;; and https://gerrit-review.googlesource.com/Documentation/user-search-accounts.html#_search_operators
                     (gerrit-rest-sync "GET" nil (format "/accounts/?q=is:active&o=DETAILS&S=%s" start-idx))))
                (setq accounts (append
                                accounts
                                (mapcar (lambda (account-info)
                                          (cons (alist-get '_account_id account-info)
                                                (alist-get 'username account-info)))
                                        response)))
                (setq start-idx (+ start-idx (length response)))
                ;; (message "start: %s" start-idx)
                (setq continue (alist-get '_more_accounts (car (last response))))))
            accounts)
        (error '())))

    ;;
    ;; override upload functions so that it ignores unknown transients instead of failing
    ;; what a bad choice, fucc ur shitty package ! TODO: complain in yet another issue
    ;;
    (defun gerrit-upload:--action (&optional args)
      "Push the current changes/commits to the gerrit server and set metadata."
      (interactive
       (list (transient-args 'gerrit-upload-transient)))

      (gerrit--ensure-commit-msg-hook-exists)
      ;; TODO check that all to-be-uploaded commits have a changeid line

      (let (assignee
            push-opts
            (remote (gerrit-get-remote))
            (refspec (gerrit-upload--get-refspec)))
        ;; there are a bunch of push options that are supported by gerrit:
        ;; https://gerrit-review.googlesource.com/Documentation/user-upload.html#push_options

        ;; I don't like this handling of transient-args, maybe transient can
        ;; pass alists to gerrit-upload--action instead of a list of strings

        ;; TODO use code from https://github.com/raxod502/apheleia/pull/56/files
        (cl-loop for arg in args do
                 (cond ((s-starts-with? "reviewers=" arg)
                        (cl-loop for reviewer in (s-split "," (s-chop-prefix "reviewers=" arg)) do
                                 ;; TODO check that reviewers are valid (by checking that all
                                 ;; reviewers don't contain a white-space)
                                 (push (concat "r=" reviewer) push-opts)))
                       ((s-starts-with? "assignee=" arg)
                        (setq assignee (s-chop-prefix "assignee=" arg)))
                       ((s-starts-with? "topic=" arg)
                        (push  arg push-opts))
                       ((string= "ready" arg)
                        (push "ready" push-opts))
                       ((string= "wip" arg)
                        (push "wip" push-opts))))

        (when push-opts
          (setq refspec (concat refspec "%" (s-join "," push-opts))))

        (gerrit-push-and-assign
         assignee
         "--no-follow-tags" ;; don't error when encountering local tags, which
                            ;; are absent from gerrit.
         remote
         (concat "HEAD:"  refspec))))

    ;;
    ;; add advice that enables skipping the pre push hook by setting an env var before
    ;; calling the upload function and then unsetting it after
    ;;
    (defadvice gerrit-upload:--action (around advice-gerrit-upload:--action activate)
      (interactive)
      (if (called-interactively-p 'any)
	  (progn
            (cl-loop for arg in (transient-args 'gerrit-upload-transient) do
                     (cond ((string= "skip-checks" arg)
                            (setenv "GIT_SKIP_PRE_PUSH_HOOK" "1"))))
            (call-interactively (ad-get-orig-definition 'gerrit-upload:--action))
            (setenv "GIT_SKIP_PRE_PUSH_HOOK" nil))
        ad-do-it))

    (transient-insert-suffix 'gerrit-upload-transient (list 0 -1)
      '("s" "Skip checks" "skip-checks"))

    ;; abanonded this in favor of a skip argument which ended up being
    ;; way more involved lmao.
    ;;
    ;; (transient-insert-suffix 'gerrit-upload-transient (list 1 -1)
    ;;   '("s" "Skip checks" my/gerrit-upload:--skip-pre-push))
    ;;
    ;; (defun my/gerrit-upload:--skip-pre-push ()
    ;;   (interactive)
    ;;   (setenv "GIT_SKIP_PRE_PUSH_HOOK" "1")
    ;;   (gerrit-upload:--action)
    ;;   (setenv "GIT_SKIP_PRE_PUSH_HOOK" nil))
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
