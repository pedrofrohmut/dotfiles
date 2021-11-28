;;; UI ##########################################################################

;; Clean up UI
(tool-bar-mode 0)    ; Hide toolbar
(menu-bar-mode 0)    ; Hide menubar
(scroll-bar-mode 0)  ; Hide scrollbar
(tooltip-mode 0)     ; Disable tooltips
(set-fringe-mode 7)  ; Give some padding

;; Remove startup screen
(setq inhibit-startup-message t)

;; Theme & Customization ########################################################

;; Set theme - Comment it in a fresh install or doom-themes not installed
(load-theme 'doom-palenight t)

;; Set transparency
(set-frame-parameter (selected-frame) 'alpha '(90 90))

;; Set font family/size
(set-frame-font "FiraCode Nerd Font Mono 12" nil t)

;; Color Column/Ruler
(global-display-fill-column-indicator-mode t)
(setq-default display-fill-column-indicator-column 81)

;; Blinking ui as bell
(setq visible-bell nil)

;; Relative line numbers
(global-display-line-numbers-mode t)
(setq-default display-line-numbers-type 'relative)

;; Diable line numbers to certain modes
(dolist (mode '(term-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Hightlight Matching parens on hover
(show-paren-mode t)

;; Disable backup files
(setq make-backup-files nil)

;; MELPA ########################################################################

;; Adding packages
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("elpa" . "https://elpa.gnu.org/packages/") t)
(package-initialize)

;; USE-PACKAGE ##################################################################

;; Install Use-Package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Start & config
(require 'use-package)
(setq use-package-always-ensure t)

;; Emmet ########################################################################

;; Emmet
(use-package emmet-mode
  :defer t
  :init
  (add-hook 'css-mode-hook 'emmet-mode)
  (add-hook 'sgml-mode-hook 'emmet-mode)
  :config
  (setq emmet-self-closing-tag-style " /")
  (setq-default emmet-move-cursor-between-quote t)
  ;(unbind-key "C-j" emmet-mode-keymap)
  (unbind-key "<C-return>" emmet-mode-keymap)
  (unbind-key "C-M-<left>" emmet-mode-keymap)
  (unbind-key "C-M-<right>" emmet-mode-keymap))

;; Evil #########################################################################

;; Evil Mode
(use-package evil
  :init
  (setq evil-want-C-u-scroll t)
  (setq evil-want-keybinding nil) ; Evil collection asks for it
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  :config
  ;; Ctrl-h as backspace
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)
  ;; Ctrl-l as delete
  (define-key evil-insert-state-map (kbd "C-l") 'evil-delete-char)
  ;; Change window normal mode (next)
  (define-key evil-normal-state-map (kbd "C-j") 'evil-window-next)
  ;; Change window normal mode (previous)
  (define-key evil-normal-state-map (kbd "C-k") 'evil-window-prev)
  ;; Change tab normal mode (next)
  (define-key evil-normal-state-map (kbd "C-l") 'tab-next)
  ;; Change tab normal mode (previous)
  (define-key evil-normal-state-map (kbd "C-h") 'tab-previous)
  ;; Easy insert linebreak in normal mode
  (define-key evil-normal-state-map (kbd "RET") (kbd "i RET <escape>"))
  ;; Removed to be used by EasyMotion + Sneak
  (define-key evil-normal-state-map (kbd "s") nil)
  ;; Removed to be used by EasyMotion + Sneak (reverse)
  (define-key evil-normal-state-map (kbd "S") nil)
  ;; Removed to be used by projectile
  (define-key evil-normal-state-map (kbd "C-p") nil)
  ;; Removed to be used by treemacs
  (define-key evil-normal-state-map (kbd "C-n") nil)
  (evil-mode 1))

;; Evil - Must Have #############################################################

;; Makes evil keys consistent in more places than just evil mode default
(use-package evil-collection
  :after
  evil
  :config
  (evil-collection-init))

;; Evil Commentary 'gc<motion>' 'gcc' ...
(use-package evil-commentary
  :after
  evil
  :config
  (evil-commentary-mode 1))

;; Evil Surround (emulate tim pope)
(use-package evil-surround
  :after
  evil
  :config
  (global-evil-surround-mode t))

;; Evil-numbers
(use-package evil-numbers
  :after
  evil
  :config
  ;; Evil Increase hovered number
  (define-key evil-normal-state-map (kbd "C--") 'evil-numbers/dec-at-pt)
  (define-key evil-normal-state-map (kbd "<kp-subtract>") 'evil-numbers/dec-at-pt)
  ;; Evil Decrease hovered number
  (define-key evil-normal-state-map (kbd "C-=") 'evil-numbers/inc-at-pt)
  (define-key evil-normal-state-map (kbd "<kp-add>") 'evil-numbers/inc-at-pt))

;; Evil Jumping #################################################################

;; Vim Sneak Emualtion
(use-package evil-snipe
  :after
  evil
  :custom
  (evil-snipe-scope 'visible)
  :config
  (evil-define-key '(normal motion) evil-snipe-local-mode-map
    "s" nil
    "S" nil)
  (evil-snipe-mode 1))

;; Vim Easy Motion Emulation
(use-package evil-easymotion
  :after
  evil
  :config
  (evilem-default-keybindings "C-SPC")
  (evilem-define (kbd "s") 'evil-snipe-repeat
                 :pre-hook (save-excursion (call-interactively #'evil-snipe-s))
                 :bind ((evil-snipe-scope 'buffer)
                        (evil-snipe-enable-highlight)
                        (evil-snipe-enable-incremental-highlight)))
  (evilem-define (kbd "S") 'evil-snipe-repeat-reverse
                 :pre-hook (save-excursion (call-interactively #'evil-snipe-s))
                 :bind ((evil-snipe-scope 'buffer)
                        (evil-snipe-enable-highlight)
                        (evil-snipe-enable-incremental-highlight))))

;; Whick-Key ####################################################################

;; Show buffer with keys
(use-package which-key
  :init
  (which-key-mode)
  :diminish
  (which-key-mode)
  :config
  (setq which-key-idle-delay 1.0)
  (which-key-setup-side-window-right))

;; Ivy, Counsel and Swiper ######################################################

(use-package ivy
  :diminish ; Does not show the mode in the mode line
  :bind
  (("C-s" . swiper)
   :map ivy-minibuffer-map
   ("TAB" . ivy-alt-done)
   ("C-l" . ivy-alt-done)
   ("C-j" . ivy-next-line)
   ("C-k" . ivy-previous-line)
   :map ivy-switch-buffer-map
   ("C-k" . ivy-previous-line)
   ("C-l" . ivy-done)
   ("C-d" . ivy-switch-buffer-kill)
   :map ivy-reverse-i-search-map
   ("C-k" . ivy-previous-line)
   ("C-d" . ivy-reverse-i-search-kill))
  :config
  (setq ivy-ignore-buffers '("\\` " "~" "\\`\\*tramp/"))
  (ivy-mode 1))

(use-package counsel
  :after ivy
  :bind
  (("M-x" . counsel-M-x)
   ("C-x b" . counsel-ibuffer)
   ("C-x C-f" . counsel-find-file)
   :map minibuffer-local-map
   ("C-r" . 'counsel-minibuffer-history))
  :config
  (counsel-mode 1))

(use-package swiper
  :after ivy)

(use-package ivy-rich
  :after ivy
  :init
  (ivy-rich-mode 1))

;; Doom Styling #################################################################

(use-package doom-modeline
  :ensure t
  :init
  (doom-modeline-mode 1)
  :custom
  (doom-modeline-height 10))

(use-package doom-themes)

;; Helpful  #####################################################################
;; Improve emacs default help and remap help commands

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command]  . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key]      . helpful-key))

;; General ######################################################################
;; easier set key binds
(use-package general
  :config
  (general-evil-setup t))

(general-nmap
  :prefix "SPC"
  "SPC" '(counsel-M-x :which-key "M-x")
  ;; Buffers
  "b"   '(:igonre t :which-key "Buffers")
  "b b" '(ibuffer :which-key "IBuffer")
  "b j" '(next-buffer :which-key "Next Buffer")
  "b k" '(previous-buffer :which-key "Previous Buffer")
  "b k" '(kill-buffer :which-key "Kill Buffer")
  "b d" '(kill-current-buffer :which-key "Kill Current Buffer")
  ;; Files
  "f"   '(:ignore t :which-key "Files")
  "f f" '(counsel-find-file :which-key "Find File")
  ;; Projectile
  "p"   '(projectile-command-map :which-key "Projectile Commands")
  ;; Tabs
  "t"   '(:ignore t :which-key "Tabs")
  "t l" '(tab-next :which-key "Go Next")
  "t h" '(tab-previous :which-key "Go Previous")
  "t n" '(tab-new :which-key "New Tab")
  "t c" '(tab-close :which-key "Close Tab")
  ;; Window
  "w"   '(:ignore t :which-key "Window")
  "w c" '(evil-window-delete :which-key "Close Window")
  "w s" '(evil-window-split :which-key "Split")
  "w v" '(evil-window-vsplit :which-key "Vertical Split")
  "w h" '(evil-window-left :which-key "Go Left")
  "w j" '(evil-window-down :which-key "Go Down")
  "w k" '(evil-window-up :which-key "Go Up")
  "w l" '(evil-window-right :which-key "Go Right")
  "w r" '(evil-window-rotate-downwards :which-key "Rotate")
  "w o" '(delete-other-windows :which-key "Close Others"))
 
;; Projectile ###################################################################
(use-package projectile
  :diminish
  projectile-mode
  :init
  (setq projectile-project-search-path '("~/programming/" "~/dotfiles/"))
  (setq projectile-switch-project-action #'projectile-dired)
  :config
  (projectile-mode 1)
  :custom
  (projectile-completion-system 'ivy)
  :bind (:map projectile-mode-map
              ("C-p" . projectile-find-file)
              ("C-c p" . projectile-command-map)))

(use-package counsel-projectile
  :config
  (counsel-projectile-mode))

;; Git ##########################################################################

(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package forge)

(org-babel-do-load-languages
 'org-babel-load-languages '((emacs-lisp . t)
                             (python . t)))
;; Org Mode #####################################################################

;; Org Tempo - Shortcuts to code blocks in Org Mode
(require 'org-tempo)

(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))

(defun pf/org-mode-setup ()
  (org-indent-mode)
  (visual-line-mode 1)
  (auto-fill-mode 0)
  (setq evil-mode-auto-indent nil))

(defun pf/org-replace-list-hyphen-with-dot ()
  (font-lock-add-keywords
   'org-mode
   '(("^ *\\([-]\\) "
      (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•")))))))

(defun pf/org-set-faces-for-heading-levels ()
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Cantarell" :weight 'Bold :height (cdr face))))

(defun pf/org-ensure-fixed-pitch-when-needed ()
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

(defun pf/org-setup-font ()
  (pf/org-replace-list-hyphen-with-dot)
  (pf/org-set-faces-for-heading-levels)
  (pf/org-ensure-fixed-pitch-when-needed))

(defun pf/configure-org-agenda ()
  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-agenda-files
        '("~/documents/org-files/tasks.org"
          "~/documents/org-files/birthdays.org")))

(use-package org
  :config
  (setq org-ellipsis " \202"     ; Change the 3 dots to down arrow 
        org-hide-emphasis-markers t)
  (pf/configure-org-agenda)
  (pf/org-setup-font)
  :bind
  (:map org-mode-map
        ("<C-j>" . nil)
        ("<C-k>" . nil))
  :hook
  (org-mode . pf/org-mode-setup))

(use-package org-bullets
  :after
  org
  :hook
  (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(use-package visual-fill-column
  :config
  (setq-default visual-fill-column-width 120)
  (setq-default visual-fill-column-center-text t)
  :config
  (global-visual-fill-column-mode 1))

;; LSP ##########################################################################

(use-package lsp-mode
  :commands
  (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook
  (python-mode . lsp)
  (typescript-mode . lsp)
  (lsp-mode . lsp-enable-which-key-integration))

;; Lsp Sideline, Peek, Doc and IMenu
(use-package lsp-ui
  :commands
  lsp-ui-mode
  :hook
  (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom)
  (lsp-ui-doc-delay 1.5))

(use-package lsp-ivy
  :commands
  lsp-ivy-workspace-symbol)

;; (use-package lsp-treemacs :commands lsp-treemacs-errors-list)
;; ;; optionally if you want to use debugger
;; (use-package dap-mode)
;; ;; (use-package dap-LANGUAGE) to load the dap adapter for your language

(use-package company
  :after
  lsp-mode
  :hook
  (lsp-mode . company-mode)
  :bind
  (:map company-active-map
    ("<tab>" . company-complete-selection)
  (:map lsp-mode-map
    ("<tab>" . company-indent-or-complete-common)))
  :custom
  (company-minimun-prefix-length 1)
  (company-idle-delay 0.0))

(use-package company-box
  :hook
  (company-mode . company-box-mode))

(use-package flycheck
  :init (global-flycheck-mode))

;; TypeScript ###################################################################

;; Server: $ npm install --global typescript
(use-package typescript-mode
  :mode
  "\\.ts\\'"
  :hook
  (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2))

;; Python #######################################################################

;; Server: pip install --user 'python-lsp-server[all]'
(use-package lsp-pyright
    :hook
    (python-mode . lsp-deferred))

(use-package jinja2-mode)

;; Haskell ######################################################################
(use-package haskell-mode)

; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Ready Ctrl + h, j, k, l to my navigation as in VIM and XMonad
(global-unset-key (kbd "C-h"))  ; Can still use help with F1
(global-unset-key (kbd "C-l"))  ; Can use evil zz 
;(global-unset-key (kbd "C-j"))  ; Not useful before
;(global-unset-key (kbd "C-k"))  ; Not useful either

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(haskell-mode jinja2-mode which-key visual-fill-column use-package typescript-mode org-bullets lsp-ui lsp-pyright lsp-ivy ivy-rich helpful general forge flycheck evil-surround evil-snipe evil-numbers evil-easymotion evil-commentary evil-collection emmet-mode doom-themes doom-modeline counsel-projectile company-box)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
