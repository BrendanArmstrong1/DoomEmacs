;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Brendan Armstrong"
      user-mail-address "Brendan.Armstrong@mail.com")

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
(setq evil-goggles-duration 0.4)
(setq evil-collection-setup-minibuffer t)
(setq evil-want-minibuffer t)
(setq doom-theme 'doom-vibrant)
(setq doom-vibrant-brighter-modeline t)
(setq doom-vibrant-padded-modeline 1)
(custom-theme-set-faces! 'doom-vibrant
  '(default :background "#101010")
  '(hl-line :background "#101010")
  '(solaire-default-face :background "#101010"))
;; "monospace" means use the system default. However, the default is usually two
;; points larger than I'd like, so I specify size 12 here.
(setq doom-font (font-spec :family "FiraCode Nerd Font Mono" :size 16)
      doom-variable-pitch-font (font-spec :family "Noto Serif" :size 15)
      doom-big-font (font-spec :family "FiraCode Nerd Font Mono" :size 25)
      ivy-posframe-font (font-spec :family "FiraCode Nerd Font Mono" :size 15))

;; Prevents some cases of Emacs flickering
(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))
(add-to-list 'default-frame-alist '(alpha . 85 ))

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")



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
(setq-default custom-file (expand-file-name ".custom.el" doom-private-dir))
(when (file-exists-p custom-file)
  (load custom-file))

;; basic settings
(setq evil-vsplit-window-right t
      evil-split-window-below t)

(setq company-idle-delay nil)

;; When I bring up Doom's scratch buffer with SPC x, it's often to play with
;; elisp or note something down (that isn't worth an entry in my org files). I
;; can do both in `lisp-interaction-mode'.
(setq doom-scratch-initial-major-mode 'lisp-interaction-mode)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type `relative)

(after! undo-tree
  (setq undo-tree-auto-save-history nil))
;; Disable invasive lsp-mode features
(setq lsp-ui-sideline-enable nil   ; not anymore useful than flycheck
      lsp-ui-doc-enable nil        ; slow and redundant with K
      lsp-enable-symbol-highlighting nil
      ;; If an LSP server isn't present when I start a prog-mode buffer, you
      ;; don't need to tell me. I know. On some systems I don't care to have a
      ;; whole development environment for some ecosystems.
      +lsp-prompt-to-install-server 'quiet)

(map! :after evil
        :map global-map
        "C-j" #'evil-window-down
        "C-k" #'evil-window-up
        "C-h" #'evil-window-left
        "C-l" #'evil-window-right)
(map! :after evil
      :map evil-window-map
      "C-q" #'evil-quit)
(map! :after evil
   :nv "C-a" #'evil-numbers/inc-at-pt
   :nv "C-M-a" #'evil-numbers/dec-at-pt)
(use-package! evil-matchit
  :after evil
  :config
  (global-evil-matchit-mode 1))
(use-package! evil-textobj-line
  :after evil)
