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

(setq! my/toggle-dir (concat doom-private-dir "toggle/"))

(setq! my/org-dir (if (file-exists-p! "cut" my/toggle-dir)
                     "~/cut/org/"
                   "~/org/"))
(setq! my/daily-dir (concat my/org-dir "daily/"))
(setq! my/citation-dir (concat my/org-dir "cite/"))
(setq! my/citation-bib (concat my/citation-dir "zot.bib"))

(setq! org-directory my/org-dir)
(setq! org-roam-directory my/org-dir)
(setq! org-ref-notes-directory my/citation-dir)
(setq! org-agenda-files (list my/daily-dir my/citation-dir))

;; to enable toggles:
;; $ touch ~/.doom.d/toggle/<toggle-name>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (file-exists-p! "arista" my/toggle-dir)
  (load! "arista.el" doom-private-dir))

(when (file-exists-p! "org" my/toggle-dir)
  (load! "org.el" doom-private-dir))

;; look / feel
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq doom-theme 'doom-molokai)
(setq display-line-numbers-type nil)
(setq confirm-kill-emacs nil)
(setq! visual-fill-column-width 100)

;; escape hook
(setq doom-escape-hook (remove 'yas-abort-snippet doom-escape-hook))

;; when yassnippet starts hijacking org roam dailies, try this
;; (set-file-template! 'org-mode :ignore t)

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
    '(("^CAPTURE-.*\\.org$" :autosave 'ignore :size 0.4)))

  ;; wrap text at 100 columns in org files
  (add-hook! org-mode #'visual-fill-column-mode))

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
  '((org-list-dt org-level-1 org-level-2 org-level-3 org-level-4 org-level-5 org-level-6 org-level-7 org-level-8)
    :background nil :foreground "brightwhite" :weight bold )
  '((org-link org-roam-link org-roam-link-current ) :background nil :foreground "#8fc3ff" :weight normal)
  '((org-link-invalid org-roam-link-shielded) :background nil :foreground "brightred" :weight normal)
  '(org-date :foreground "white" :background "#101010" )
  '((org-tag org-tag-group) :foreground "#9c91e4"))

;; keybinds
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; leader keys
(map! :leader
      :desc "M-x" ";" #'counsel-M-x
      :desc "Eval expression" ":" #'pp-eval-expression
      :desc "Search dir" "?" #'+default/search-other-cwd
      (:prefix "w" ; window
       "-" #'evil-window-split
       "\\" #'evil-window-vsplit
       "+" nil)
      (:prefix "TAB"
       "j" #'+workspace/switch-left
       "k" #'+workspace/switch-right)
      (:prefix "b"
       :desc "Open scratch buffer" "x" #'doom/switch-to-scratch-buffer
       "X" nil)
      (:prefix "p"
       :desc "Open scratch buffer" "x" #'doom/switch-to-project-scratch-buffer
       "X" nil))

;buffer / window management
(map! :map ivy-minibuffer-map "C-M-k" #'ivy-switch-buffer-kill)

(map! :nmiv "C-o" #'evil-window-next
      (:map (compilation-mode-map compilation-minor-mode-map) "C-o" nil)
      (:after help :map help-mode-map :nm "C-o" nil))

(map! :nmvg "C-b" #'ivy-switch-buffer
      (:map magit-mode-map :nv "C-b" nil)
      (:map counsel-find-file-map "C-b"
       (lambda ()
         (interactive)
         (ivy-exit-with-action
          (lambda (_)
            (ivy-switch-buffer))))))

(defun my/find-file-in-dir (&optional directory)
  (interactive "D")
    (let ((file (ivy-completing-read "Find file: "
                                     (projectile-dir-files directory))))
      (find-file (expand-file-name file directory))))

(map! :nmvg "C-f" #'counsel-find-file
      (:map magit-mode-map :nv "C-f" nil)
      (:map counsel-find-file-map "C-f"
       (lambda ()
         (interactive)
         (ivy-exit-with-action
          (lambda (str)
            (my/find-file-in-dir str)))))
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
(map! :map (compilation-mode-map
            backtrace-mode-map
            view-mode-map
            special-mode-map)
      "h" nil)
(map! :nmv "j" #'evil-next-visual-line)
(map! :nmv "k" #'evil-previous-visual-line)
(map! :nmvi "C-k" (lambda () (interactive) (evil-scroll-line-down 8))
      :nmvi "C-j" (lambda () (interactive) (evil-scroll-line-up 8))
      (:after (evil-org org) :map (org-mode-map evil-org-mode-map)
       :nmiv "C-k" nil
       :nmiv "C-j" nil)
      (:after magit :map magit-mode-map
       :nm "C-k" nil
       :nm "C-j" nil)
      (:map grep-mode-map
       :n "C-k" nil
       :n "C-j" nil)
      (:map comint-mode-map
       :n "C-j" nil
       :n "C-k" nil)
      (:after sql :map sql-interactive-mode-map
       "C-j" nil))
(map! :nm "C-d" (lambda () (interactive) (evil-scroll-line-up (/ (window-height) 2)))
      (:after rjsx-mode :map rjsx-mode-map :nm "C-d" nil)
      (:after magit :map magit-mode-map :nm "C-d" nil)
      (:map comint-mode-map :ng "C-d" nil))
(map! :nm "C-u" (lambda () (interactive) (evil-scroll-line-down (/ (window-height) 2))))
(map! :nmv "J" (kbd "3j"))
(map! :nmv "K" (kbd "3k")
      (:after magit :map magit-mode-map
       "K" nil))
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
       "C-p" #'magit-section-backward)
      (:map grep-mode-map
       :n "C-n" #'next-error-no-select
       :n "C-p" #'previous-error-no-select ))

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
