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

;; my variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq! my/doom-private-dir (expand-file-name doom-private-dir))
(setq! my/toggle-dir (concat my/doom-private-dir "toggle/"))
(setq! my/json-schema-dir (concat my/doom-private-dir "json-schemas/"))

;; to enable toggles:
;; $ touch ~/.doom.d/toggle/<toggle-name>
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (file-exists-p! "arista" my/toggle-dir)
  (load! "arista.el" doom-private-dir))

;; behavior
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq confirm-kill-emacs nil)

(after! avy
  (setq avy-all-windows t))

(after! swiper
  (setq swiper-action-recenter nil))

(after! ivy
  (setq ivy-use-virtual-buffers t)
  (setq ivy-xref-use-file-path t))

(after! evil
  (setq evil-move-cursor-back nil)
  (setq evil-cross-lines t))

(after! yasnippet
  (setq doom-escape-hook (remove 'yas-abort-snippet doom-escape-hook)))

(after! counsel
  (setq counsel-rg-base-command "rg -M 240 --with-filename --no-heading --line-number --color never %s || true"))

(use-package! caddyfile-mode
  :mode (("Caddyfile\\'" . caddyfile-mode)
         ("caddy\\.conf\\'" . caddyfile-mode)))

(after! compile

  ;; add support for eslint compilation
  (defun compile-eslint--find-filename ()
    "Find the filename for current error."
    (save-match-data
      (save-excursion
        (when (re-search-backward (rx bol (group "/" (+ any)) eol))
          (list (match-string 1))))))
  (let ((form `(eslint
                ,(rx-to-string
                  '(and (group (group (+ digit)) ":" (group (+ digit)))
                        (+ " ") (or "error" "warning")))
                compile-eslint--find-filename
                2 3 2 1)))
    (if (assq 'eslint compilation-error-regexp-alist-alist)
        (setf (cdr (assq 'eslint compilation-error-regexp-alist-alist)) (cdr form))
      (push form compilation-error-regexp-alist-alist)))
  (push 'eslint compilation-error-regexp-alist)
  )

;; TODO: magit blame window???
;; https://github.com/redguardtoo/vc-msg
;; https://www.reddit.com/r/emacs/comments/e93p51/better_git_blame_messages/
;; https://github.com/seagle0128/.emacs.d/blob/master/lisp/init-vcs.el#L138

(defun setup-web-indent (n)
  ;; web development
  (setq javascript-indent-level n) ; javascript-mode
  (setq js-indent-level n) ; js-mode
  (setq js2-basic-offset n) ; js2-mode, alias of js-indent-level
  (setq web-mode-markup-indent-offset n) ; web-mode, html tag in html file
  (setq web-mode-css-indent-offset n) ; web-mode, css in html file
  (setq web-mode-code-indent-offset n) ; web-mode, js code in html file
  (setq css-indent-offset n) ; css-mode
  )
(setup-web-indent 2)
(add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))

;; prepend annoying buffer names with their parent dir, ie:
;; index.js -> parent-dir/index.js
(defun rename-buffers-with-annoying-names ()
  (when (member (buffer-name) '("index.js"
                                "package.json"
                                "docker-compose.yaml"
                                "Dockerfile"
                                "style.less"
                                "constants.js"
                                "constants.less"
                                ))
    (when (string-match "[^/]+/[^/]+$" (buffer-file-name))
      (rename-buffer (match-string 0 (buffer-file-name)) t))))
(add-hook 'find-file-hook #'rename-buffers-with-annoying-names)

;; lsp customization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; javascript LSP notes:
;;
;; xref will only show references in loaded buffers unless you add the
;; following file to the root of the javascript project. be sure to
;; exclude the node modules directory
;;
;; jsconfig.json
;;
;; {
;;   "exclude": ["node_modules"]
;; }

(setq lsp-json-schemas
      `[(:fileMatch ["caddy.json"]
         :url ,(concat my/json-schema-dir "caddy_schema.json"))])

;; appearance
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq doom-theme 'doom-molokai)
(setq display-line-numbers-type nil)

;; popup stuff
(plist-put +popup-defaults :modeline t)

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
  )

;; keybinds
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; activate mouse-based scrolling
(unless (display-graphic-p)
  (map! "<mouse-4>" #'evil-scroll-line-up
        "<mouse-5>" #'evil-scroll-line-down))

; leader keys
(map! :leader
      :desc "M-x" ";" #'counsel-M-x
      :desc "Eval expression" ":" #'pp-eval-expression
      :desc "Search dir" "?" #'+default/search-other-cwd
      :desc "Maximize window" "z" #'doom/window-maximize-buffer
      :desc "Undo window changes" "Z" #'winner-undo
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
(map! :after counsel :map counsel-find-file-map
      "C-h" #'counsel-up-directory
      "C-l" #'counsel-down-directory)

(map! :nmiv "C-o" #'evil-window-next
      (:map (compilation-mode-map compilation-minor-mode-map) "C-o" nil)
      (:after help :map help-mode-map :nm "C-o" nil))

; switch buffer
(defun my/ivy-switch-buffer-inital-input (input)
  "Switch to another buffer."
  (interactive)
  (ivy-read "Switch to buffer: " #'internal-complete-buffer
            :initial-input input
            :keymap ivy-switch-buffer-map
            :preselect (buffer-name (other-buffer (current-buffer)))
            :action #'ivy--switch-buffer-action
            :matcher #'ivy--switch-buffer-matcher
            :caller #'ivy-switch-buffer))
(defun my/ivy-switch-buffer-git ()
  (interactive)
  (my/ivy-switch-buffer-inital-input "magit\\|gerrit "))

(map! :nmvg "C-b" #'+ivy/switch-buffer
      (:map ivy-switch-buffer-map "C-b" #'ignore)
      (:after magit :map magit-mode-map
       :nv "C-b" #'my/ivy-switch-buffer-git)
      (:map (counsel-find-file-map ivy-minibuffer-map) "C-b"
       (lambda ()
         (interactive)
         (ivy-exit-with-action
          (lambda (_)
            (+ivy/switch-buffer))))))

; find file
(setq! my/find-file-recursive-prompt "Find file (recursive): ")
(defun my/find-file-recursive (&optional directory)
  (interactive "D")
    (let ((file (ivy-completing-read my/find-file-recursive-prompt
                                     (projectile-dir-files-native directory))))
      (find-file (expand-file-name file directory))))
;; ensure that calling projectile-dir-files exists when above function is called
(autoload #'projectile-dir-files-native "projectile")

(map! :nmvg "C-f" #'counsel-find-file
      (:map magit-mode-map :nv "C-f" nil)
      (:map ivy-minibuffer-map "C-f"
       (lambda ()
         (interactive)
         (ivy-exit-with-action
          (lambda (str)
            (counsel-find-file)))))
      (:map counsel-find-file-map "C-f"
       (lambda ()
         (interactive)
         (ivy-exit-with-action
          (lambda (str)
            (my/find-file-recursive str)))))
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
      (:map (compilation-mode-map compilation-minor-mode-map)
       :n "C-j" nil
       :n "C-k" nil )
      (:map transient-map
       "C-j" #'transient-scroll-down
       "C-k" #'transient-scroll-up)
      (:after sql :map sql-interactive-mode-map
       "C-j" nil))

;; this is so fucking stupid. evil-collection is annoying as hell
(after! (:and evil-collection git-timemachine)
  (evil-define-minor-mode-key 'normal 'git-timemachine-mode
    "\C-k" nil
    "\C-j" nil))

(map! :nm "C-d" (lambda () (interactive) (evil-scroll-line-up (/ (window-height) 2)))
      (:after rjsx-mode :map rjsx-mode-map :nm "C-d" nil)
      (:after magit :map magit-mode-map :nm "C-d" nil)
      (:map comint-mode-map :ng "C-d" nil))
(map! :nm "C-u" (lambda () (interactive) (evil-scroll-line-down (/ (window-height) 2))))
(map! :nmv "J" (kbd "3j")
      :nmv "K" (kbd "3k")
      (:after magit :map magit-mode-map
       "K" nil
       "J" nil))
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
       :n "C-p" #'previous-error-no-select )
      (:map (compilation-mode-map compilation-minor-mode-map)
       :n "C-n" #'compilation-next-error
       :n "C-p" #'compilation-previous-error )
     )

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
      (:map org-mode-map :n "gj" nil)
      (:map git-commit-mode-map :nv "gj" nil))
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
