;; UI ##########################################################################

;; Clean up UI
(tool-bar-mode 0)    ; Hide toolbar
(menu-bar-mode 0)    ; Hide menubar
(scroll-bar-mode 0)  ; Hide scrollbar
(tooltip-mode 0)     ; Disable tooltips
(set-fringe-mode 7)  ; Give some padding

;; Remove startup screen
(setq inhibit-startup-message t)

;; Theme & Customization #######################################################

;; Set color theme
(load-theme 'doom-palenight t)

;; Set transparency
(set-frame-parameter (selected-frame) 'alpha '(90 90))

;; Set font family/size
(set-frame-font "FiraCode Nerd Font Mono 12" nil t)

;; Color Column/Ruler
(global-display-fill-column-indicator-mode t)
(setq-default display-fill-column-indicator-column 121)

;; Blinking ui as bell
(setq visible-bell t) 

;; Relative line numbers
(global-display-line-numbers-mode t)
(setq display-line-numbers-type 'relative)

;; Diable line numbers to certain modes
(dolist (mode '(term-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Hightlight Matching parens on hover
(show-paren-mode t)

;; MELPA #######################################################################

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("elpa" . "https://elpa.gnu.org/packages/") t)
(package-initialize)

;; Use-Package #################################################################

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; Evil ########################################################################

;; Evil Mode
(use-package evil
  :init
  (setq evil-want-C-u-scroll t)
  (setq evil-want-keybinding nil) ; Evil collection asks for it
  :config
  ; Ctrl-k as Esc
  (define-key evil-insert-state-map (kbd "C-k") 'evil-normal-state)
  ; Ctrl-h as backspace
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)
  ; Ctrl-l as delete
  (define-key evil-insert-state-map (kbd "C-l") 'evil-delete-char)
  ; Easy insert linebreak in normal mode
  (define-key evil-normal-state-map (kbd "RET") (kbd "i RET <escape>"))
  (evil-mode 1))

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

;; Keybinds ####################################################################

(global-set-key (kbd "<escape>") 'keyboard-escape-quit) ; Make ESC quit prompts

;; Ready Ctrl + h, j, k, l to my navigation as in VIM and XMonad
(global-unset-key (kbd "C-h"))  ; Can still use help with F1
(global-unset-key (kbd "C-l"))  ; Can use evil zz 
(global-unset-key (kbd "C-j"))  ; Not useful before
(global-unset-key (kbd "C-k"))  ; Not useful either

;; Use-package auto-generated code #############################################

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(evil-surround evil-commentary evil-collection evil use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
