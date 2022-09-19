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
(load-theme 'doom-tokyo-night t)

;; Set transparency
(set-frame-parameter (selected-frame) 'alpha '(90 90))

;; Set font family/size
(set-frame-font "FiraCode Nerd Font Mono 12" nil t)

;; Color Column/Ruler
(global-display-fill-column-indicator-mode t)
(setq-default display-fill-column-indicator-column 101)

;; Blinking ui as bell
(setq visible-bell nil)

;; Tab to 4 spaces
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)


;; Relative line numbers
(global-display-line-numbers-mode t)
(setq-default display-line-numbers-type 'relative)
(set-face-foreground 'line-number "#505049")

;; Diable line numbers to certain modes
(dolist (mode '(term-mode-hook
                eshell-mode-hook))
    (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Hightlight Matching parens on hover
(show-paren-mode t)

;; Disable backup files
(setq make-backup-files nil)

;; Show cursor position in the status bar
(setq column-number-mode t)

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

;; Doom Styling #################################################################

(use-package doom-modeline
    :ensure t
    :init
    (doom-modeline-mode 1)
    :custom
    (doom-modeline-height 10))

(use-package doom-themes)

;; Evil #########################################################################

;; Evil Mode
(use-package evil
  :init
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-u-delete t)
  (setq evil-want-C-i-jump t)
  (setq evil-want-keybinding nil) ; Evil collection asks for it
  (setq evil-want-Y-yank-to-eol t)
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  (setq evil-cross-lines t)
  (setq evil-move-beyond-eol t)
  :config
  ;; Ctrl-h as backspace
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)
  ;; Ctrl-l as delete
  (define-key evil-insert-state-map (kbd "C-l") 'evil-delete-char)
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
    ;;; Removed to be used by projectile
                                        ;(define-key evil-normal-state-map (kbd "C-p") nil)
    ;;; Removed to be used by treemacs
                                        ;(define-key evil-normal-state-map (kbd "C-n") nil)
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
  ;; Evil Decrease hovered number
  (define-key evil-normal-state-map (kbd "C-=") 'evil-numbers/inc-at-pt))

;; Vertigo Completion ###########################################################

(use-package vertico
  :custom
  (vertico-cycle t)
  :init
  (vertico-mode))

;; Show details for the items in vertico (file size, permissions, etc)
(use-package marginalia
  :after vertico
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

;; Custom #######################################################################

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(marginalia use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
