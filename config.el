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
(setq my-org-dir (if (file-exists-p! "macc" doom-private-dir)
                     "~/cut/org"
                   "~/org"))
(setq citation-dir (concat my-org-dir "/cite"))
(setq citation-bib (concat citation-dir "/zot.bib"))

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

(unless (display-graphic-p)
  ;; activate mouse-based scrolling
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
  (setq! org-directory my-org-dir))

(after! bibtex
  (setq! bibtex-completion-notes-path citation-dir
         bibtex-completion-bibliography citation-bib
         ivy-bibtex-default-action #'ivy-bibtex-edit-notes))

(use-package! org-ref
    :after org
    :config
    (org-ref-ivy-cite-completion)
    (setq! org-ref-default-bibliography (list citation-bib)
           org-ref-notes-directory citation-dir
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
        :head "#+TITLE: ${author} - ${title}\n#+ROAM_KEY: ${ref}\n#+ROAM_TAGS: citation\n#+CREATED: %u\n\n* INBOX\n* ARCHIVE\n"
        :unnarrowed t))))

(after! org-roam
  (setq org-roam-directory my-org-dir)
  (setq org-roam-index-file "index.org")
  (setq org-roam-capture-templates
        '(("n" "note" plain #'org-roam-capture--get-point
           :file-name "${slug}"
           :head "#+TITLE: ${title}\n#+ROAM_ALIAS:\n#+ROAM_TAGS:\n#+CREATED: %u\n\n- related ::\n\n"
           :unnarrowed t)))
  (setq my/org-roam-capture-templates
        '(("c" "cite" item #'org-roam-capture--get-point
           "- %? (pg. ${page-number})"
           :file-name "${slug}"
           :head "#+TITLE: ${title}\n#+ROAM_ALIAS:\n#+ROAM_TAGS:\n#+CREATED: %u\n\n- related ::\n\n"
           :olp ("INBOX")
           :empty-lines 1)))
  (setq org-roam-dailies-capture-templates
        '(("x" "fleet" item #'org-roam-capture--get-point
           :file-name "daily/%<%Y-%m-%d>"
           :head "#+TITLE: %<%Y-%m-%d %a>\n#+ROAM_TAGS: daily\n\n* JOURNAL\n\n\n* INBOX\n* ARCHIVE"
           :olp ("INBOX")
           :empty-lines 1)
          ("j" "journal" item #'org-roam-capture--get-point
           :file-name "daily/%<%Y-%m-%d>"
           :head "#+TITLE: %<%Y-%m-%d %a>\n#+ROAM_TAGS: daily\n\n* JOURNAL\n\n\n* INBOX\n* ARCHIVE"
           :olp ("JOURNAL")
           :empty-lines 1)))

  ;; custom org-roam capture stuff
  (defun my/org-roam-capture--capture (&optional goto keys)
    (let* ((org-capture-templates (mapcar #'org-roam-capture--convert-template my/org-roam-capture-templates))
           (one-template-p (= (length org-capture-templates) 1))
           org-capture-templates-contexts
           (org-capture-link-is-already-stored t))
      (when one-template-p
        (setq keys (caar org-capture-templates)))
      (if (or one-template-p
              (eq org-roam-capture-function 'org-capture))
          (org-capture goto keys)
        (funcall-interactively org-roam-capture-function))))
  (defun my/org-roam--get-title-path-completions-tag-filtered (tag-filter)
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
          (let ((v (list title :path file-path :title title)))
            (if (member tag-filter tags) (push v completions)))))))
  (defun my/org-roam-capture--tag-filtered (tag-filter &optional goto keys)
    (interactive "P")
    (unless org-roam-mode (org-roam-mode))
    (let* ((completions (my/org-roam--get-title-path-completions-tag-filtered tag-filter))
           (title-with-keys (org-roam-completion--completing-read "File: "
                                                                  completions
                                                                  :require-match t))
           (res (cdr (assoc title-with-keys completions)))
           (title (or (plist-get res :title) title-with-keys))
           (file-path (plist-get res :path)))
      (let ((org-roam-capture--info (list (cons 'title title)
                                          (cons 'slug (funcall org-roam-title-to-slug-function title))
                                          (cons 'file file-path)))
            (org-roam-capture--context 'capture))
        (condition-case err
            (my/org-roam-capture--capture goto keys)
          (error (user-error "%s.  Please adjust `org-roam-capture-templates'"
                             (error-message-string err)))))))
  (defun my/org-roam-capture-existing-citation (&optional goto keys)
    (interactive "P")
    (my/org-roam-capture--tag-filtered "citation" goto keys))

  ;; custom org-roam-dailies capture stuff
  (defun my/org-roam-dailies--capture (time &optional goto keys)
    (unless org-roam-mode (org-roam-mode))
    (let ((org-roam-capture-templates (--> org-roam-dailies-capture-templates
                                           (if goto (list (car it)) it)))
          (org-roam-capture--info (list (cons 'time time)))
          (org-roam-capture--context 'dailies))
      (org-roam-capture--capture (when goto '(4)) keys)))
  (defun my/org-roam-dailies-capture-today-fleet (&optional goto keys)
    (interactive "P")
    (my/org-roam-dailies--capture (current-time) nil "x"))
  (defun my/org-roam-dailies-capture-today-journal (&optional goto keys)
    (interactive "P")
    (my/org-roam-dailies--capture (current-time) nil "j")))


(custom-set-faces!
  '(eros-result-overlay-face :background nil)
  '(ivy-modified-buffer :foreground nil :inherit font-lock-doc-face)
  '(swiper-line-face :background "color-235" :foreground nil )
  '(avy-goto-char-timer-face :background "red" :foreground nil)
  '(avy-lead-face :background "red" :foreground "white")
  '(avy-lead-face-0 :background "red" :foreground "white")
  '(avy-lead-face-1 :background "red" :foreground "white")
  '(avy-lead-face-2 :background "red" :foreground "white")
  '(show-paren-match :background "brightblack" :foreground nil))

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
       :desc "Cited Fleet" "c" #'my/org-roam-capture-existing-citation
       :desc "Journal Entry" "j" #'my/org-roam-dailies-capture-today-journal
       :desc "Note" "n" #'org-roam-capture)
      (:prefix "TAB"
       "j" #'+workspace/switch-left
       "k" #'+workspace/switch-right)
      (:prefix "b"
       :desc "Open scratch buffer" "x" #'doom/switch-to-scratch-buffer
       "X" nil)
      (:prefix "p"
       :desc "Open scratch buffer" "x" #'doom/switch-to-project-scratch-buffer
       "X" nil))

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
       :desc "Store link" "s" #'org-store-link
       :desc "Show backlinks" "b" #'org-roam
       (:prefix ("i" . "Insert")
        :desc "note" "n" #'org-roam-insert
        :desc "citation" "c" #'orb-insert
        :desc "link" "l" #'org-insert-link
        :desc "last stored link" "s" #'org-insert-last-stored-link)
       (:prefix ("d" . "Dailies")
        :desc "Date" "d" #'org-roam-dailies-find-date
        :desc "Today" "t" #'org-roam-dailies-find-today
        :desc "Tomorrow" "m" #'org-roam-dailies-find-tomorrow
        :desc "Yesterday" "y" #'org-roam-dailies-find-yesterday
        :desc "Prev" "h" #'org-roam-dailies-find-previous-note
        :desc "Next" "l" #'org-roam-dailies-find-next-note)
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
      "RET" nil
      "C-SPC" #'company-complete-selection
      "C-@" #'company-complete-selection) ; C-@ is terminal bind for C-SPC

; cursor nav
(map! :map compilation-mode-map "h" nil)
(map! :nmv "j" #'evil-next-visual-line)
(map! :nmv "k" #'evil-previous-visual-line)
(map! :nmv "C-k" (lambda () (interactive) (evil-scroll-line-down 8))
      :i "C-k" #'evil-force-normal-state
      :nmv "C-j" (lambda () (interactive) (evil-scroll-line-up 8))
      :i "C-j" #'evil-force-normal-state
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
(map! :nm "C-n" nil
      :nm "C-p" nil
      (:after org :map org-mode-map
       "C-n" #'org-next-visible-heading
       "C-p" #'org-previous-visible-heading)
      (:after magit :map magit-mode-map
       "C-n" #'magit-section-forward
       "C-p" #'magit-section-backward))
(map! :g "C-p" #'evil-paste-after)

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
(map! :nmv "gj" #'evil-join)
(map! :nmv "g;" #'comment-line)
(map! :nmv "go" (lambda () (interactive) (+evil/insert-newline-below 1) (evil-next-visual-line)))
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
