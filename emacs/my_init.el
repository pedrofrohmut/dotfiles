;; Set color theme
(load-theme 'doom-palenight t)

;; Set transparency
(set-frame-parameter (selected-frame) 'alpha '(90 90))
(add-to-list 'default-frame-alist '(alpha 90 90))

;; Set font family/size
(set-frame-font "FiraCode Nerd Font Mono 11" nil t)

;; Remove startup screen
(setq inhibit-startup-message t)

;; Clean up UI
(tool-bar-mode 0)    ; Hide toolbar
(menu-bar-mode 0)    ; Hide menubar
(scroll-bar-mode 0)  ; Hide scrollbar
(tooltip-mode 0)     ; Disable tooltips
(set-fringe-mode 7)  ; Give some padding

(setq visible-bell t) ; Blinking ui as bell

;; Relative line numbers
(global-display-line-numbers-mode t)
(setq display-line-numbers-type 'relative)

;; Diable line numbers to certain modes
(dolist (mode '(org-mode-hook
		term-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Hightlight Matching parens on hover
(show-paren-mode t)

;; Initialize package sources
(require 'package)

(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Install use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; Evil Mode
(use-package evil
  :init
  (setq evil-want-C-u-scroll t)
  (setq evil-want-keybinding nil) ; Evil collection asks for it
  :config
  (define-key evil-insert-state-map (kbd "C-k") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)
  (define-key evil-insert-state-map (kbd "C-l") 'evil-delete-char)
  (evil-mode 1))

;; Makes evil keys consistent in more places than just evil mode default
(use-package evil-collection
  :after
  evil
  :config
  (evil-collection-init))

;; Evil Commentary 'gc<motion>' 'gcc' ...
(use-package evil-commentary
  :init
  (evil-commentary-mode 1))

;; Show buffer with keys
(use-package which-key
  :init 
  (which-key-mode)
  :diminish
  (which-key-mode)
  :config
  (setq which-key-idle-delay 1.2)
  (setq which-key-show-early-on-C-h t)
  (which-key-setup-side-window-right))

;; Emmet
(use-package emmet-mode
  :defer t
  :init
  (add-hook 'css-mode-hook 'emmet-mode)
  (add-hook 'sgml-mode-hook 'emmet-mode)
  :config
  (setq emmet-self-closing-tag-style " /")
  (setq-default emmet-move-cursor-between-quote t)
  (unbind-key "<C-return>" emmet-mode-keymap)
  (unbind-key "C-M-<left>" emmet-mode-keymap)
  (unbind-key "C-M-<right>" emmet-mode-keymap))

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

(use-package doom-modeline
  :ensure t
  :init
  (doom-modeline-mode 1)
  :custom
  (doom-modeline-height 10))

(use-package doom-themes)

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command]  . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key]      . helpful-key))

(use-package general
  :config
  (general-evil-setup t)
  (general-create-definer rune/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")
  (rune/leader-keys
    "t"  '(:ignore t :which-key "toggles")
    "tt" '(counsel-load-theme :which-key "choose theme")
    "f"  '(:ignore t :which-key "files")
    "ff" '(counsel-find-file :which-key "find file")))

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
              ("C-p" . projectile-command-map)
              ("C-c p" . projectile-command-map)))

(use-package counsel-projectile
  :config
  (counsel-projectile-mode))

(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package forge)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(global-display-line-numbers-mode t)
 '(ivy-mode t)
 '(package-selected-packages
   '(forge evil-commentary evil-magit magit counsel-projectile projectile evil-collection general doom-themes helpful counsel ivy-rich rainbow-delimiters emmet-mode which-key evil doom-modeline use-package ivy command-log-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

; # Keybinds ####################################################################
(global-set-key (kbd "<escape>") 'keyboard-escape-quit) ; Make ESC quit prompts
(global-set-key (kbd "M-b") 'counsel-switch-buffer)     ; Switch to buffer

; # General Keybinds ############################################################
(general-define-key
  "C-c b" 'counsel-switch-buffer)
