;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq!
        user-full-name "Brendan Armstrong"
        user-mail-address "Brendan.Armstrong@mail.com"
        mu4e-get-mail-command "mbsync -c ~/.config/isync/mbsyncrc -a"
        auth-sources '("~/.authinfo.gpg")
        ;; Org stuff
        org-directory "~/org/"
        org-archive-location (concat org-directory ".archive/Archive.org::datetree/")
        ;; Roam settings
        org-roam-directory (concat org-directory "roam/")
        org-roam-db-location (concat org-roam-directory ".org-roam.db")
        org-roam-index-file (concat org-roam-directory "Index.org")
        ;; Journal settings
        forge-topic-list-limit '(100 . -10)
        display-line-numbers-type 'relative
        doom-scratch-initial-major-mode 'lisp-interaction-mode
        evil-want-minibuffer t
        company-idle-delay nil
        evil-vsplit-window-right t
        evil-split-window-below t
        evil-goggles-duration 0.4
        doom-theme 'doom-vibrant
        doom-vibrant-brighter-modeline t
        doom-vibrant-padded-modeline 1
        doom-font (font-spec :family "Hack Nerd Font Mono" :size 16 :height 1.0)
        doom-variable-pitch-font (font-spec :family "Noto Serif" :height 1.0)
        doom-big-font (font-spec :family "Hack Nerd Font Mono" :size 26))

(setq-default custom-file "/dev/null")

;; Theme stuff
(custom-theme-set-faces! 'doom-vibrant
        '(default :background "#101010")
        '(hl-line :background "#101010")
        '(solaire-default-face :background "#101010"))
(after! doom-themes
        (setq doom-themes-enable-bold t
                doom-themes-enable-italic t))
(set-face-attribute
        '(font-lock-comment-face :slant italic)
        '(font-lock-keyword-face :slant italic))
(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))
(add-to-list 'default-frame-alist '(alpha . 85 ))
(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-footer)
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))


;; Other packages
(after! undo-tree
  (setq undo-tree-auto-save-history nil))

(use-package! evil-matchit
        :after evil
        :config
        (global-evil-matchit-mode 1))

(use-package! evil-textobj-line
        :after evil)

;;Latex stuff

(setq +latex-viewers '(pdf-tools))
(map! :map cdlatex-mode-map
    :i "TAB" #'cdlatex-tab)

;; Projectile

(after! projectile
  (setq! projectile-switch-project-action #'projectile-dired))

;; Magit

(after! magit
  (setq! magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))


(after! rustic
  (setq rustic-lsp-server 'rls))

(setq lsp-clients-clangd-args '("-j=3"
                                "--background-index"
                                "--clang-tidy"
                                "--completion-style=detailed"
                                "--header-insertion=never"
                                "--header-insertion-decorators=0"))
(after! lsp-clangd (set-lsp-priority! 'clangd 2))
;; Org stuff


(after! org
  (setq!
        org-journal-dir (concat org-directory "Journal/")
        org-journal-enable-agenda-integration t
        org-journal-date-prefix "#+TITLE: "
        org-journal-time-prefix "* "
        org-journal-date-format "%a, %Y-%m-%d"
        org-journal-file-format "%Y-%m-%d.org"))

(add-hook! org-mode #'org-journal--update-org-agenda-files)

(use-package! org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(add-hook! 'org-mode-hook #'+org-pretty-mode #'auto-fill-mode)
(add-hook! 'org-mode-hook (company-mode t))
(add-hook! 'org-capture-mode-hook (company-mode t))

(after! org
  (setq org-agenda-files '("~/org/OrgFiles/Tasks.org"
                        "~/org/OrgFiles/Birthdays.org"
                        "~/org/OrgFiles/Habits.org")))
(after! org
  (setq!
        org-refile-targets '((nil :maxlevel . 1) (org-agenda-files :maxlevel . 1))
        org-agenda-span 21
        org-ellipsis " ▾"
        org-tags-column -80
        org-startup-with-inline-images nil
        org-link-search-must-match-exact-headline nil
        org-image-actual-width 400
        org-agenda-start-with-log-mode t
        +org-capture-todo-file "~/org/OrgFiles/Tasks.org"
        +org-capture-notes-file "~/org/OrgFiles/Notes.org"
        org-capture-templates
        '(("t" "Personal todo" entry
        (file+headline +org-capture-todo-file "Inbox")
        "* TODO %?\n%i" :empty-lines 1 :kill-buffer t)
        ("T" "Personal todo with capture link" entry
        (file+headline +org-capture-todo-file "Inbox")
        "* TODO %?\n%i\n%a" :empty-lines 1 :kill-buffer t)
        ("n" "Personal notes" entry
        (file+datetree +org-capture-notes-file)
        "* %u %?\n%i\n%a" :empty-lines 1 :kill-buffer t))
        org-log-done 'time
        org-log-into-drawer t
        org-agenda-window-setup 'other-window)
  (add-to-list 'org-modules 'org-habit)
  (advice-add 'org-refile :after 'org-save-all-org-buffers)
  (advice-add 'org-archive-subtree :after 'org-save-all-org-buffers))


(map!   :after evil-org
        :map evil-org-mode-map
        :i "RET" #'evil-org-return
        :nv "M-J" #'org-metadown
        :nv "M-K" #'org-metaup
        :nv "M-j" #'org-shiftmetadown
        :nv "M-k" #'org-shiftmetaup
        :nv "C-j" #'org-forward-element
        :nv "C-k" #'org-backward-element
        :nv "C-l" #'org-down-element
        :nv "C-h" #'org-up-element)

;; Org Roam stuff

(after! org
  (setq!
   +org-roam-open-buffer-on-find-file nil))


;; Mappings

(map! :after evil
      :nv "j" #'evil-next-visual-line
      :nv "k" #'evil-previous-visual-line)

(map!   :after vertico
        :map vertico-map
        :nv "j" #'vertico-next
        :nv "k" #'vertico-previous
        :nv "RET" #'vertico-exit
        :nv "TAB" #'vertico-insert
        :nv "y" #'vertico-save)

(map!
        :after evil
        :map evil-window-map
        "C-j" #'evil-window-down
        "C-k" #'evil-window-up
        "C-h" #'evil-window-left
        "C-l" #'evil-window-right
        "j" #'+evil/window-move-down
        "k" #'+evil/window-move-up
        "h" #'+evil/window-move-left
        "l" #'+evil/window-move-right
        "C-q" #'evil-quit)

(map!
        :after evil
        :localleader
        :map dired-mode-map
        "f" #'dired-create-empty-file)

(map!
        :leader
        :nv "DEL" #'org-mark-ring-goto
        :nv "?" #'+default/search-project
        :nv "/" #'+default/search-buffer)

