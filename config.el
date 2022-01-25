;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-


(setq
        user-full-name "Brendan Armstrong"
        user-mail-address "Brendan.Armstrong@mail.com"
        mu4e-get-mail-command "mbsync -c ~/.config/isync/mbsyncrc -a"
        auth-sources '("~/.authinfo.gpg")
        org-directory "~/org/"
        forge-topic-list-limit '(100 . -10)
        lsp-ui-sideline-enable nil   ; not anymore useful than flycheck
        lsp-ui-doc-enable nil        ; slow and redundant with K
        ;; If an LSP server isn't present when I start a prog-mode buffer, you
        ;; don't need to tell me. I know. On some systems I don't care to have a
        ;; whole development environment for some ecosystems.
        lsp-enable-symbol-highlighting nil
        +lsp-prompt-to-install-server 'quiet
        display-line-numbers-type `relative
        doom-scratch-initial-major-mode 'lisp-interaction-mode
        evil-want-minibuffer t
        company-idle-delay nil
        evil-vsplit-window-right t
        evil-split-window-below t
        evil-goggles-duration 0.4
        doom-theme 'doom-vibrant
        doom-vibrant-brighter-modeline t
        doom-vibrant-padded-modeline 1
        doom-font (font-spec :family "Hack Nerd Font Mono" :size 16)
        doom-variable-pitch-font (font-spec :family "Noto Serif" :size 16)
        doom-big-font (font-spec :family "Hack Nerd Font Mono" :size 25))

(custom-theme-set-faces! 'doom-vibrant
        '(default :background "#101010")
        '(hl-line :background "#101010")
        '(solaire-default-face :background "#101010"))
(after! doom-themes
        (setq doom-themes-enable-bold t
                doom-themes-enable-italic t))
(custom-set-faces!
        '(font-lock-comment-face :slant italic)
        '(font-lock-keyword-face :slant italic))
(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))
(add-to-list 'default-frame-alist '(alpha . 85 ))


(setq-default custom-file (expand-file-name ".custom.el" doom-private-dir))
(when (file-exists-p custom-file)
  (load custom-file))
(after! undo-tree
  (setq undo-tree-auto-save-history nil))

(map!   :after evil
        :map org-mode-map
        :nv "C-j" #'org-forward-element
        :nv "C-k" #'org-backward-element
        :nv "C-l" #'org-down-element
        :nv "C-h" #'org-up-element)

(map!   :after evil
        :map vertico-map
        :nv "j" #'vertico-next
        :nv "k" #'vertico-previous
        :nv "RET" #'vertico-exit
        :nv "TAB" #'vertico-insert
        :nv "y" #'vertico-save)

(map!
        :after evil
        :map global-map
        "C-M-j" #'evil-window-down
        "C-M-k" #'evil-window-up
        "C-M-h" #'evil-window-left
        "C-M-l" #'evil-window-right)

(map!
        :after evil
        :map evil-window-map
        "C-j" #'+evil/window-move-down
        "C-k" #'+evil/window-move-up
        "C-h" #'+evil/window-move-left
        "C-l" #'+evil/window-move-right
        "C-q" #'evil-quit)

(map!
        :after evil
        :localleader
        :map dired-mode-map
        "f" #'dired-create-empty-file)

(map!
        :leader
        :nv "?" #'+default/search-project
        :nv "/" #'+default/search-buffer)

(use-package! evil-matchit
        :after evil
        :config
        (global-evil-matchit-mode 1))

(use-package! evil-textobj-line
        :after evil)
