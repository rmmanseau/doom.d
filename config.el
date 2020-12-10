;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Ryan Manseau"
      user-mail-address "ryan@arista.com")

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
(setq doom-theme 'doom-molokai)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type nil)

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

;; optional config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load! "arista.el" doom-private-dir t)

;; misc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(unless window-system
  (xterm-mouse-mode t)
  (global-set-key (kbd "<mouse-4>") 'scroll-down-line)
  (global-set-key (kbd "<mouse-5>") 'scroll-up-line))

;; after package config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(after! avy
  (setq avy-all-windows t)
  (set-face-attribute 'avy-goto-char-timer-face nil :background "color-88" :foreground 'unspecified)
  (set-face-attribute 'avy-lead-face nil :background "color-88" :foreground "white")
  (set-face-attribute 'avy-lead-face-0 nil :background "color-88" :foreground "white")
  (set-face-attribute 'avy-lead-face-1 nil :background "color-88" :foreground "white")
  (set-face-attribute 'avy-lead-face-2 nil :background "color-88" :foreground "white"))
(after! swiper
  (set-face-attribute 'swiper-line-face nil :background "color-240" :foreground 'unspecified ))
(after! ivy
  (setq ivy-use-virtual-buffers t))
(after! evil
  (setq evil-move-cursor-back nil)
  (setq evil-cross-lines t))

;; keybinds
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(map!
 :leader
 (:prefix "w" ; window
  "-" #'evil-window-split
  "\\" #'evil-window-vsplit

  "+" nil
  )
 :desc "M-x" ";" #'counsel-M-x
 :desc "Eval expression" ":" #'pp-eval-expression
 )

(map! :map ivy-minibuffer-map "C-M-k" #'ivy-switch-buffer-kill)

(map!
 ; window / mode nav
 :nmiv "C-o" #'evil-window-next
 (:map compilation-mode-map "C-o" nil)
 :n "C-b" #'ivy-switch-buffer
 :n "C-f" #'counsel-find-file
 :iv "C-g" #'evil-force-normal-state
 :iv "C-j" #'evil-force-normal-state

 ; cursor nav
 :nmv "j" #'evil-next-visual-line
 :nmv "k" #'evil-previous-visual-line
 :n "C-k" (lambda () (interactive) (evil-scroll-line-down 8))
 :n "C-j" (lambda () (interactive) (evil-scroll-line-up 8))
 :nmv "J" (kbd "4j")
 :nmv "K" (kbd "4k")
 :nmv "L" #'evil-forward-WORD-end
 :nmv "H" #'evil-backward-WORD-begin
 :mnv "ga" #'evil-avy-goto-char-timer
 :mnv "gl" #'evil-avy-goto-line
 :n "gb" #'better-jumper-jump-backward
 :n "gf" #'better-jumper-jump-forward

 ; utilities
 :n "/" #'swiper
 :n "*" #'swiper-thing-at-point
 :n "U" #'evil-redo
 )
