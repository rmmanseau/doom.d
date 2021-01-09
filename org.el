;;; org.el -*- lexical-binding: t; -*-

(map! :leader
      "X" nil
      "x" nil
      (:prefix ("x" . "Capture")
       :desc "Fleet" "x" #'my/org-roam-dailies-capture-today-fleet
       :desc "Journal" "j" #'my/org-roam-dailies-capture-today-journal
       (:prefix ("c" . "Cite")
        :desc "Cite" "c" #'my/org-roam-capture-existing-citation
        :desc "Page" "p" #'my/org-roam-capture-existing-citation-page
        :desc "Time" "t" #'my/org-roam-capture-existing-citation-time
        :desc "Memo" "m" #'my/org-roam-capture-existing-citation-memo
        )
       :desc "Note" "n" #'org-roam-capture
       :desc "Bib Note" "b" #'ivy-bibtex)
      (:prefix "n"
       "i" nil
       "d" nil
       "n" nil
       "N" nil
       "S" nil
       "c" nil
       "C" nil
       "o" nil
       "v" nil
       "m" nil
       "l" nil
       :desc "Search notes" "/" #'+default/org-notes-search
       :desc "Today" "x" #'org-roam-dailies-find-today
       :desc "Notes" "n" #'org-roam-find-file
       :desc "Citations" "c" #'ivy-bibtex
       :desc "Browse files" "F" #'+default/browse-notes
       :desc "Show backlinks" "b" #'org-roam
       :desc "Date" "d" #'org-roam-dailies-find-date
       :desc "Fleets" "f" (lambda () (interactive) (org-todo-list "FLEET"))))

(map! :map org-mode-map
      "M-n" nil
      (:prefix ("M-n" . "Note functions")
       :desc "previous daily" "M-h" #'org-roam-dailies-find-previous-note
       :desc "next daily" "M-l" #'org-roam-dailies-find-next-note
       :desc "show backlinks" "M-n" #'org-roam
       :desc "store link" "M-s" #'org-store-link
       :desc "build cache" "M-b" #'org-roam-db-build-cache
       :desc "created property" "M-c" #'my/org-set-created-property
       (:prefix ("M-i" . "Insert")
        :desc "note" "n" #'org-roam-insert
        :desc "citation" "c" #'orb-insert
        :desc "link" "l" #'org-insert-link
        :desc "last stored link" "s" #'org-insert-last-stored-link
        :desc "header" "h" #'org-insert-heading
        :desc "header" "j" #'org-insert-subheading)))

(defvar my/org-created-property-name "CREATED"
  "The name of the org-mode property that stores the creation date of the entry")

(defun my/org-set-created-property (&optional active NAME)
  "Set a property on the entry giving the creation time.

By default the property is called CREATED. If given the `NAME'
argument will be used instead. If the property already exists, it
will not be modified."
  (interactive)
  (let* ((created (or NAME my/org-created-property-name))
         (fmt (if active "<%s>" "[%s]"))
         (now  (format fmt (format-time-string "%Y-%m-%d %a %H:%M"))))
    (unless (org-entry-get (point) created nil)
      (org-set-property created now))))

(after! org
  (add-hook! 'org-capture-before-finalize-hook #'my/org-set-created-property)
  ;; (setq! org-agenda-sorting-strategy `((agenda timestamp-down habit-down priority-down category-keep)
  ;;                                      (todo priority-down category-keep)
  ;;                                      (tags priority-down category-keep)
  ;;                                      (search category-keep)))
  ;; (setq! org-agenda-sorting-strategy-selected '(timestamp-down))
  (setq org-todo-keywords '((sequence "TODO(t)" "WAIT(w)" "|" "DONE(d)")
                            (sequence "FLEET(f)" "|" "ARCHIVE(a)")))
  (setq! org-capture-templates
         '(("t" "Personal todo" entry
            (file+headline +org-capture-todo-file "Inbox")
            "* [ ] %?\n%i\n%a" :prepend t)
           ("n" "Personal notes" entry
            (file+headline +org-capture-notes-file "Inbox")
            "* %u %?\n%i\n%a" :prepend t)
           ("j" "Journal" entry
            (file+olp+datetree +org-capture-journal-file)
            "* %U %?\n%i\n%a" :prepend t)
           ("p" "Templates for projects")
           ("pt" "Project-local todo" entry
            (file+headline +org-capture-project-todo-file "Inbox")
            "* TODO %?\n%i\n%a" :prepend t)
           ("pn" "Project-local notes" entry
            (file+headline +org-capture-project-notes-file "Inbox")
            "* %U %?\n%i\n%a" :prepend t)
           ("pc" "Project-local changelog" entry
            (file+headline +org-capture-project-changelog-file "Unreleased")
            "* %U %?\n%i\n%a" :prepend t)
           ("o" "Centralized templates for projects")
           ("ot" "Project todo" entry #'+org-capture-central-project-todo-file "* TODO %?\n %i\n %a" :heading "Tasks" :prepend nil)
           ("on" "Project notes" entry #'+org-capture-central-project-notes-file "* %U %?\n %i\n %a" :heading "Notes" :prepend t)
           ("oc" "Project changelog" entry #'+org-capture-central-project-changelog-file "* %U %?\n %i\n %a" :heading "Changelog" :prepend t))))

(after! bibtex
  (setq! bibtex-completion-notes-path my/citation-dir
         bibtex-completion-bibliography my/citation-bib
         ivy-bibtex-default-action #'ivy-bibtex-edit-notes))

(use-package! org-ref
    :after org
    :config
    (org-ref-ivy-cite-completion)
    (setq! org-ref-default-bibliography (list my/citation-bib)
           org-ref-notes-function #'orb-edit-notes-ad))

(use-package! org-roam-bibtex
    :after org-roam
    :hook (org-roam-mode . org-roam-bibtex-mode)
    :config
    (setq!
     orb-insert-link-description 'citekey
     orb-insert-interface 'ivy-bibtex
     orb-templates
     '(("r" "ref" plain #'org-roam-capture--get-point
        "%?"
        :file-name "cite/${citekey}"
        :head "#+TITLE: ${citekey} [ ${title}, ${author} ]\n#+ROAM_KEY: ${ref}\n#+ROAM_TAGS: citation\n#+CREATED: %u\n\n* FLEETING\n\n\n* MEMO\n"
        :unnarrowed t))))

(after! org-roam
  (setq org-roam-index-file "index.org")
  (setq +org-roam-open-buffer-on-find-file nil)
  (setq org-roam-capture-templates
        '(("n" "note" plain #'org-roam-capture--get-point
           "%?\n"
           :file-name "${slug}"
           :head "#+TITLE: ${title}\n#+ROAM_ALIAS:\n#+ROAM_TAGS:\n#+CREATED: %u\n\n- related ::\n\n"
           :unnarrowed t)))
  (setq my/org-roam-capture-templates
        '(("c" "citation" entry #'org-roam-capture--get-point
           "** FLEET %?\n"
           :file-name "${slug}"
           :olp ("FLEETING")
           :empty-lines 1)
          ("t" "timestamp" entry #'org-roam-capture--get-point
           "** FLEET (ts. ${time-stamp}) %?\n"
           :file-name "${slug}"
           :olp ("FLEETING")
           :empty-lines 1)
          ("p" "page" entry #'org-roam-capture--get-point
           "** FLEET (pg. ${page-number}) %?\n"
           :file-name "${slug}"
           :olp ("FLEETING")
           :empty-lines 1)
          ("m" "memo" item #'org-roam-capture--get-point
           "+ %?\n"
           :file-name "${slug}"
           :olp ("MEMO")
           :empty-lines 1)))
  (setq org-roam-dailies-capture-templates
        '(("x" "fleet" entry #'org-roam-capture--get-point
           "** FLEET %?\n"
           :file-name "daily/%<%Y-%m-%d>"
           :head "#+TITLE: %<%Y-%m-%d %a>\n#+CREATED: %u\n#+ROAM_TAGS: daily\n\n* FLEETING\n\n\n* JOURNAL\n"
           :olp ("FLEETING")
           :empty-lines 1)
          ("j" "journal" item #'org-roam-capture--get-point
           "+ %?\n"
           :file-name "daily/%<%Y-%m-%d>"
           :head "#+TITLE: %<%Y-%m-%d %a>\n#+CREATED: %u\n#+ROAM_TAGS: daily\n\n* FLEETING\n\n\n* JOURNAL\n"
           :olp ("JOURNAL")
           :empty-lines 1)))

  ;; custom org-roam capture stuff
  ;; check if patch stuff worked with (symbol-function 'function-name)
  (el-patch-defun org-roam-capture--capture (&optional goto keys (el-patch-add templates))
    "Create a new file, and return the path to the edited file.
The templates are defined at `org-roam-capture-templates'.  The
GOTO and KEYS argument have the same functionality as
`org-capture'."
    (let* ((org-capture-templates (mapcar #'org-roam-capture--convert-template
                                          (el-patch-swap org-roam-capture-templates
                                                         (if (null templates)
                                                             org-roam-capture-templates
                                                           templates))))
           (one-template-p (= (length org-capture-templates) 1))
           org-capture-templates-contexts)
      (when one-template-p
        (setq keys (caar org-capture-templates)))
      (if (or one-template-p
              (eq org-roam-capture-function 'org-capture))
          (org-capture goto keys)
        (funcall-interactively org-roam-capture-function))))

  (el-patch-defun org-roam--get-title-path-completions ((el-patch-add &optional tag-filter))
    "Return an alist for completion.
The car is the displayed title for completion, and the cdr is a
plist containing the path and title for the file."
    (let* ((rows (org-roam-db-query [:select [files:file titles:title tags:tags files:meta] :from titles
                                     :left :join tags
                                     :on (= titles:file tags:file)
                                     :left :join files
                                     :on (= titles:file files:file)]))
           completions)
      (setq rows (seq-sort-by (lambda (x)
                                (plist-get (nth 3 x) :mtime))
                              #'time-less-p
                              rows))
      (dolist (row rows completions)
        (pcase-let ((`(,file-path ,title ,tags) row))
          (let ((k (org-roam--prepend-tag-string title tags))
                (v (list :path file-path :title title)))
            (el-patch-swap
              (push (cons k v) completions)
              (if (null tag-filter)
                  (push (cons k v) completions)
                (if (member tag-filter tags)
                    (push (list title :path file-path :title title) completions)))))))))

  (el-patch-defun org-roam-capture (&optional goto keys (el-patch-add templates tag-filter require-match))
    "Launches an `org-capture' process for a new or existing note.
This uses the templates defined at `org-roam-capture-templates'.
Arguments GOTO and KEYS see `org-capture'."
    (interactive "P")
    (unless org-roam-mode (org-roam-mode))
    (let* ((completions (org-roam--get-title-path-completions (el-patch-add tag-filter)))
           (title-with-keys (org-roam-completion--completing-read "File: "
                                                                  completions
                                                                  (el-patch-add :require-match require-match)))
           (res (cdr (assoc title-with-keys completions)))
           (title (or (plist-get res :title) title-with-keys))
           (file-path (plist-get res :path)))
      (let ((org-roam-capture--info (list (cons 'title title)
                                          (cons 'slug (funcall org-roam-title-to-slug-function title))
                                          (cons 'file file-path)))
            (org-roam-capture--context 'capture))
        (condition-case err
            (org-roam-capture--capture goto keys (el-patch-add templates))
          (error (user-error "%s.  Please adjust `org-roam-capture-templates'"
                             (error-message-string err)))))))

  (el-patch-defun org-roam-dailies--capture (time &optional goto (el-patch-add keys))
    "Capture an entry in a daily-note for TIME, creating it if necessary.

When GOTO is non-nil, go the note without creating an entry."
    (unless org-roam-mode (org-roam-mode))
    (let ((org-roam-capture-templates (--> org-roam-dailies-capture-templates
                                           (if goto (list (car it)) it)))
          (org-roam-capture--info (list (cons 'time time)))
          (org-roam-capture--context 'dailies))
      (org-roam-capture--capture (when goto '(4)) (el-patch-add keys))))

  (defun my/org-roam-capture-existing-citation (&optional goto keys templates tag-filter require-match)
    (interactive "P")
    (org-roam-capture goto "c" my/org-roam-capture-templates "citation" t))
  (defun my/org-roam-capture-existing-citation-page (&optional goto keys templates tag-filter require-match)
    (interactive "P")
    (org-roam-capture goto "p" my/org-roam-capture-templates "citation" t))
  (defun my/org-roam-capture-existing-citation-time (&optional goto keys templates tag-filter require-match)
    (interactive "P")
    (org-roam-capture goto "t" my/org-roam-capture-templates "citation" t))
  (defun my/org-roam-capture-existing-citation-memo (&optional goto keys templates tag-filter require-match)
    (interactive "P")
    (org-roam-capture goto "m" my/org-roam-capture-templates "citation" t))
  (defun my/org-roam-dailies-capture-today-fleet (&optional goto keys)
    (interactive "P")
    (org-roam-dailies--capture (current-time) nil "x"))
  (defun my/org-roam-dailies-capture-today-journal (&optional goto keys)
    (interactive "P")
    (org-roam-dailies--capture (current-time) nil "j")))
