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
  (setq org-directory my-org-dir))

(after! org-roam
  (setq org-roam-directory my-org-dir)
  (setq org-roam-index-file "index.org")
  (setq org-roam-capture-templates
        '(("d" "default" plain (function org-roam-capture--get-point)
           "%?"
           :file-name "${slug}"
           :head "#+TITLE: ${title}\n#+ROAM_ALIAS:\n#+CREATED: %u\n\n- tags ::\n\n"
           :unnarrowed t))))

(custom-set-faces!
  '(eros-result-overlay-face :background nil)
  '(ivy-modified-buffer :foreground nil :inherit font-lock-doc-face)
  '(swiper-line-face :background "color-235" :foreground nil )
  '(avy-goto-char-timer-face :background "red" :foreground nil)
  '(avy-lead-face :background "red" :foreground "white")
  '(avy-lead-face-0 :background "red" :foreground "white")
  '(avy-lead-face-1 :background "red" :foreground "white")
  '(avy-lead-face-2 :background "red" :foreground "white")
  )

;; keybinds
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; leader keys
(map! :leader
      :desc "M-x" ";" #'counsel-M-x
      :desc "Eval expression" ":" #'pp-eval-expression
      :desc "Org capture" "x" #'org-capture
      "X" nil
      :desc "Search dir" "?" #'+default/search-other-cwd
      :desc "Search for symbol in dir" "#" #'+default/search-other-cwd
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
      (:map compilation-mode-map "C-o" nil)
      (:after help :map help-mode-map :n "C-o" nil))

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
(map! :nmv "j" #'evil-next-visual-line)
(map! :nmv "k" #'evil-previous-visual-line)
(map! :nmv "C-k" (lambda () (interactive) (evil-scroll-line-down 8))
      :i "C-k" #'evil-force-normal-state
      :nmv "C-j" (lambda () (interactive) (evil-scroll-line-up 8))
      :i "C-j" #'evil-force-normal-state)
(map! :after (evil-org org) :map (org-mode-map evil-org-mode-map)
      :nmiv "C-k" nil
      :nmiv "C-j" nil)
(map! :nm "C-d" (lambda () (interactive) (evil-scroll-line-up (/ (window-height) 2)))
      (:after rjsx-mode :map rjsx-mode-map :nm "C-d" nil))
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

(map! :textobj "b" #'evil-textobj-anyblock-inner-block #'evil-textobj-anyblock-a-block)
(map! :textobj ";" #'evilnc-inner-comment #'evilnc-outer-commenter)
(define-and-bind-text-object "l" "^\\s-*" "\\s-*$")
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
