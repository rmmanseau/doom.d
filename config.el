;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
;; (setq user-full-name "Ryan Manseau"
;;       user-mail-address "ryan@arista.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;; (setq doom-theme 'doom-molokai)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
;; (setq display-line-numbers-type nil)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; paths
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq! my-org-dir (if (file-exists-p! "macc" doom-private-dir)
                     "~/cut/org"
                   "~/org"))
(setq! citation-dir (concat my-org-dir "/cite"))
(setq! citation-bib (concat citation-dir "/zot.bib"))

(setq! org-directory my-org-dir)
(setq! org-roam-directory my-org-dir)
(setq! org-ref-notes-directory citation-dir)

;; $ touch arista
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (file-exists-p! "arista" doom-private-dir)
  (load-library "Arastra")
  (map! :leader
        (:prefix ("a" . "arista")
         "a" #'a4-add
         "e" #'a4-edit
         "r" #'a4-revert
         "b" #'a4-bug
         "s" #'a4-gid2
         "d" #'a4-gid2-defs
         ))
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

;; look / feel
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq doom-theme 'doom-molokai)
(setq display-line-numbers-type nil)
(setq confirm-kill-emacs nil)
(setq! visual-fill-column-width 100)

;; popup stuff
(plist-put +popup-defaults :modeline t)

;; activate mouse-based scrolling
(unless (display-graphic-p)
  (map! "<mouse-4>" #'evil-scroll-line-up
        "<mouse-5>" #'evil-scroll-line-down))

(after! avy
  (setq avy-all-windows t))

(after! swiper
  (setq swiper-action-recenter nil))

(after! ivy
  (setq ivy-use-virtual-buffers t))

(after! evil
  (setq evil-move-cursor-back nil)
  (setq evil-cross-lines t))

(after! org
  ;; dont create new file when capture is cancelled
  (set-popup-rules!
    '(("^CAPTURE-.*\\.org$" :autosave 'ignore)))
  (add-hook! org-mode #'visual-fill-column-mode)

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
  (setq! bibtex-completion-notes-path citation-dir
         bibtex-completion-bibliography citation-bib
         ivy-bibtex-default-action #'ivy-bibtex-edit-notes))

(use-package! org-ref
    :after org
    :config
    (org-ref-ivy-cite-completion)
    (setq! org-ref-default-bibliography (list citation-bib)
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
        :file-name "${citekey}"
        :head "#+TITLE: ${citekey} [ ${title}, ${author} ]\n#+ROAM_KEY: ${ref}\n#+ROAM_TAGS: citation\n#+CREATED: %u\n\n* INBOX\n* ARCHIVE\n"
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
        '(("c" "cite" entry #'org-roam-capture--get-point
           "** FLEET[C] : %?\n(pg. ${page-number})\n"
           :file-name "${slug}"
           :olp ("INBOX")
           :head "\n* INBOX\n* ARCHIVE\n"
           :empty-lines 1)))
  (setq org-roam-dailies-capture-templates
        '(("x" "fleet" entry #'org-roam-capture--get-point
           "** FLEET : %?\n"
           :file-name "daily/%<%Y-%m-%d>"
           :head "#+TITLE: %<%Y-%m-%d %a>\n#+ROAM_TAGS: daily\n\n* JOURNAL\n\n\n* INBOX\n* ARCHIVE\n"
           :olp ("INBOX")
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
    (org-roam-capture goto keys my/org-roam-capture-templates "citation" t))
  (defun my/org-roam-dailies-capture-today-fleet (&optional goto keys)
    (interactive "P")
    (org-roam-dailies--capture (current-time) nil "x"))
  (defun my/org-roam-dailies-capture-today-journal (&optional goto keys)
    (interactive "P")
    (org-roam-dailies--capture (current-time) nil "j")))

(custom-set-faces!
  '(eros-result-overlay-face :background nil)
  '(ivy-modified-buffer :foreground nil :inherit font-lock-doc-face)
  '(swiper-line-face :background "color-235" :foreground nil )
  '(avy-goto-char-timer-face :background "red" :foreground nil)
  '(avy-lead-face :background "red" :foreground "white")
  '(avy-lead-face-0 :background "red" :foreground "white")
  '(avy-lead-face-1 :background "red" :foreground "white")
  '(avy-lead-face-2 :background "red" :foreground "white")
  '(show-paren-match :background "brightblack" :foreground nil)

  '(org-document-title :background nil :foreground "#e2c770" :weight ultra-bold)
  '(org-default :background nil :foreground "white")
  '((org-level-1 org-level-2 org-level-3 org-level-4 org-level-5 org-level-6 org-level-7 org-level-8)
    :background nil :foreground "brightwhite" :weight bold )
  '((org-link org-roam-link org-roam-link-current ) :background nil :foreground "#8fc3ff" :weight normal)
  '((org-link-invalid org-roam-link-shielded) :background nil :foreground "brightred" :weight normal)
  '(org-date :foreground "white" :background "#101010" )
  '((org-tag org-tag-group org-list-dt) :foreground "#9c91e4")
  )

;; keybinds
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; leader keys
(map! :leader
      :desc "M-x" ";" #'counsel-M-x
      :desc "Eval expression" ":" #'pp-eval-expression
      :desc "Search dir" "?" #'+default/search-other-cwd
      "X" nil
      "x" nil
      (:prefix "w" ; window
       "-" #'evil-window-split
       "\\" #'evil-window-vsplit
       "+" nil)
      (:prefix ("x" . "Capture")
       :desc "Fleet" "x" #'my/org-roam-dailies-capture-today-fleet
       :desc "Citation Fleet" "c" #'my/org-roam-capture-existing-citation
       :desc "Note" "n" #'org-roam-capture
       :desc "Bib Note" "b" #'ivy-bibtex)
      (:prefix "TAB"
       "j" #'+workspace/switch-left
       "k" #'+workspace/switch-right)
      (:prefix "b"
       :desc "Open scratch buffer" "x" #'doom/switch-to-scratch-buffer
       "X" nil)
      (:prefix "p"
       :desc "Open scratch buffer" "x" #'doom/switch-to-project-scratch-buffer
       "X" nil))

(map!
 :map org-mode-map
 "M-n" nil
 (:prefix ("M-n" . "Note functions")
  :desc "previous daily" "M-h" #'org-roam-dailies-find-previous-note
  :desc "next daily" "M-l" #'org-roam-dailies-find-next-note
  :desc "show backlinks" "M-n" #'org-roam
  :desc "store link" "s" #'org-store-link
  :desc "build cache" "b" #'org-store-link
  (:prefix ("i" . "Insert")
   :desc "note" "n" #'org-roam-insert
   :desc "citation" "c" #'orb-insert
   :desc "link" "l" #'org-insert-link
   :desc "last stored link" "s" #'org-insert-last-stored-link
   :desc "header" "h" #'org-insert-heading
   :desc "header" "j" #'org-insert-subheading
   )
  ))

; org leader bindings
(map! :leader
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
       :desc "Find file" "f" #'+default/find-in-notes
       :desc "Browse files" "F" #'+default/browse-notes
       :desc "Show backlinks" "b" #'org-roam
       :desc "Date" "d" #'org-roam-dailies-find-date
       ))

;buffer / window management
(map! :map ivy-minibuffer-map "C-M-k" #'ivy-switch-buffer-kill)

(map! :nmiv "C-o" #'evil-window-next
      (:map compilation-mode-map "C-o" nil)
      (:after help :map help-mode-map :nm "C-o" nil))

(map! :nmvg "C-b" #'ivy-switch-buffer
      (:map magit-mode-map :nv "C-b" nil)
      (:map counsel-find-file-map "C-b"
       (lambda ()
         (interactive)
         (ivy-exit-with-action
          (lambda (_)
            (ivy-switch-buffer))))))

(map! :nmvg "C-f" #'counsel-find-file
      (:map magit-mode-map :nv "C-f" nil)
      (:map counsel-find-file-map "C-f"
       (lambda ()
         (interactive)
         (ivy-exit-with-action
          (lambda (str)
            (projectile-find-file-in-directory str)))))
      (:map ivy-switch-buffer-map "C-f"
       (lambda ()
         (interactive)
         (ivy-exit-with-action
          (lambda (str)
            (let* ((buf (get-buffer str))
                   (default-directory
                     (or (and buf (buffer-local-value 'default-directory buf))
                         default-directory)))
              (counsel-find-file)))))))

; make autocomplete popup less intrusive
(map! :after company :map company-active-map
      "C-g" nil
      "C-SPC" #'company-complete-selection
      "C-@" #'company-complete-selection) ; C-@ is terminal bind for C-SPC

; cursor nav
(map! :map compilation-mode-map "h" nil)
(map! :nmv "j" #'evil-next-visual-line)
(map! :nmv "k" #'evil-previous-visual-line)
(map! :nmvi "C-k" (lambda () (interactive) (evil-scroll-line-down 8))
      :nmvi "C-j" (lambda () (interactive) (evil-scroll-line-up 8))
      (:after (evil-org org) :map (org-mode-map evil-org-mode-map)
       :nmiv "C-k" nil
       :nmiv "C-j" nil)
      (:after magit :map magit-mode-map
       :nm "C-k" nil
       :nm "C-j" nil))
(map! :nm "C-d" (lambda () (interactive) (evil-scroll-line-up (/ (window-height) 2)))
      (:after rjsx-mode :map rjsx-mode-map :nm "C-d" nil)
      (:after magit :map magit-mode-map :nm "C-d" nil))
(map! :nm "C-u" (lambda () (interactive) (evil-scroll-line-down (/ (window-height) 2))))
(map! :nmv "J" (kbd "3j"))
(map! :nmv "K" (kbd "3k"))
(map! :nmv "L" #'evil-forward-WORD-end)
(map! :nmv "H" #'evil-backward-WORD-begin)
(map! :mnv "ga" #'evil-avy-goto-char-2)
(map! :mnv "g/" #'evil-avy-goto-char-timer)
(map! :mnv "gl" #'evil-avy-goto-line)
(map! :n "gb" #'better-jumper-jump-backward)
(map! :n "gf" #'better-jumper-jump-forward)
(map! :nmv "gc" #'goto-last-change)
(map! :nmg "C-n" nil
      :nmg "C-p" nil
      (:after org :map org-mode-map
       "C-n" #'org-next-visible-heading
       "C-p" #'org-previous-visible-heading)
      (:after magit :map magit-mode-map
       "C-n" #'magit-section-forward
       "C-p" #'magit-section-backward))

; text objects
(defmacro define-and-bind-text-object (key start-regex end-regex)
  (let ((inner-name (make-symbol "inner-name"))
        (outer-name (make-symbol "outer-name")))
    `(progn
       (evil-define-text-object ,inner-name (count &optional beg end type)
         (evil-select-paren ,start-regex ,end-regex beg end type count nil))
       (evil-define-text-object ,outer-name (count &optional beg end type)
         (evil-select-paren ,start-regex ,end-regex beg end type count t))
       (define-key evil-inner-text-objects-map ,key (quote ,inner-name))
       (define-key evil-outer-text-objects-map ,key (quote ,outer-name)))))

(define-and-bind-text-object "l" "^\\s-*" "\\s-*$")
(map! :textobj "b" #'evil-textobj-anyblock-inner-block #'evil-textobj-anyblock-a-block)
(map! :textobj ";" #'evilnc-inner-comment #'evilnc-outer-commenter)
(map! :textobj "c" nil nil)
(map! :v "v" #'er/expand-region)
(after! expand-region (setq expand-region-contract-fast-key "V"))

; editing
(map! :nmv "gj" #'evil-join
      (:map org-mode-map :n "gj" nil))
(map! :nmv "g;" #'comment-line)
(map! :n "U" #'evil-redo)

; search
(map! :nm "gm" #'+lookup/documentation)
(map! :nmv "/" #'swiper)
(map! :nmv "?" #'swiper-backward)
(map! :nmv "*" #'swiper-thing-at-point)
(map! :nmv "C-_" #'swiper-all
      :nmv "C-/" #'swiper-all
      (:map undo-fu-mode-map
      "C-/" nil
      "C-_" nil)) ; C-_ is terminal bind for C-/
(map! :after ivy
      (:map ivy-minibuffer-map
       "C-p" #'evil-paste-after
       "C-w" #'+ivy/woccur))
