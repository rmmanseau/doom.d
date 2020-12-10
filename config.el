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

(setq enable-recursive-minibuffers nil)


;; after package config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(after! avy
  (setq avy-all-windows t)
  (set-face-attribute 'avy-goto-char-timer-face nil :background "red" :foreground 'unspecified)
  (set-face-attribute 'avy-lead-face nil :background "red" :foreground "white")
  (set-face-attribute 'avy-lead-face-0 nil :background "red" :foreground "white")
  (set-face-attribute 'avy-lead-face-1 nil :background "red" :foreground "white")
  (set-face-attribute 'avy-lead-face-2 nil :background "red" :foreground "white"))
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

 ; window / mode nav
(map! :nmiv "C-o" #'evil-window-next
      (:map compilation-mode-map "C-o" nil)
      (:map help-mode-map :n "C-o" nil)
      )

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


(map! :iv "C-g" #'evil-force-normal-state)
(map! :iv "C-j" #'evil-force-normal-state)

; cursor nav
(map! :nmv "j" #'evil-next-visual-line)
(map! :nmv "k" #'evil-previous-visual-line)
(map! :n "C-k" (lambda () (interactive) (evil-scroll-line-down 8)))
(map! :n "C-j" (lambda () (interactive) (evil-scroll-line-up 8)))
(map! :nmv "J" (kbd "4j"))
(map! :nmv "K" (kbd "4k"))
(map! :nmv "L" #'evil-forward-WORD-end)
(map! :nmv "H" #'evil-backward-WORD-begin)

(map! :mnv "ga" #'evil-avy-goto-char-2)
(map! :mnv "g/" #'evil-avy-goto-char-timer)
(map! :mnv "gl" #'evil-avy-goto-line)
(map! :mnv "C-_" #'+default/search-project
      :mnv "C-/" #'+default/search-project
      (:map undo-fu-mode-map
      "C-/" nil
      "C-_" nil))
(map! :mnv "C-l" #'evil-avy-goto-line)
(map! :n "gb" #'better-jumper-jump-backward)
(map! :n "gf" #'better-jumper-jump-forward)

; utilities
(map! :n "/" #'swiper)
(map! :n "?" #'swiper-all)
(map! :n "*" #'swiper-thing-at-point)
(map! :n "U" #'evil-redo)
