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

;; This is only needed once, near the top of the file
(eval-when-compile
  (add-to-list 'load-path "~/.config/emacs/elpa")
  (setq use-package-always-ensure t)
  (require 'use-package))

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

;; Emmet ########################################################################

(use-package emmet-mode
  :defer t
  :after
  (web-mode css-mode scss-mode)
  :init
  (setq emmet-move-cursor-between-quotes t)
  (setq emmet-self-closing-tag-style " /")
  :config
  (add-hook 'emmet-mode-hook (lambda () (setq emmet-indentation 4)))
  (add-hook 'sgml-mode-hook 'emmet-mode)
  (add-hook 'css-mode-hook  'emmet-mode)
  (unbind-key "C-j" emmet-mode-keymap)
  (unbind-key "<C-return>" emmet-mode-keymap)
  (unbind-key "C-M-<left>" emmet-mode-keymap)
  (unbind-key "C-M-<right>" emmet-mode-keymap)
  :bind
  ("C-k" . emmet-expand-line))

;; Fill Column ##################################################################

(use-package visual-fill-column
  :config
  (setq-default visual-fill-column-width 101)
  (setq-default visual-fill-column-center-text t)
  :config
  (global-visual-fill-column-mode 1))

;; Syntax HightLight ############################################################

(use-package elixir-mode)

;; LSP ##########################################################################

;; (use-package lsp-mode
;;   :config
;;   (setq lsp-clients-elixir-server-executable "~/software/elixir-ls/language_server.sh")
;;   :init
;;   (setq lsp-keymap-prefix "C-c l")
;;   :hook
;;   (elixir-mode . lsp-defered)
;;   ;(lsp-mode . lsp-enable-which-key-integration)
;;   :commands
;;   (lsp lsp-defered))

;; (use-package lsp-ui
;;   :after
;;   lsp-mode
;;   :hook
;;   (lsp-mode . lsp-ui-mode)
;;   :custom
;;   (lsp-ui-doc-position 'bottom)
;;   (lsp-ui-doc-delay 1.5)
;;   :commands
;;   lsp-ui-mode)

;; (use-package lsp-ivy
;;   :after
;;   lsp-mode
;;   :commands
;;   lsp-ivy-workspace-symbol)

;(use-package lsp-treemacs :commands lsp-treemacs-errors-list)
;; optionally if you want to use debugger
;(use-package dap-mode)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language
;; optional if you want which-key integration
;(use-package which-key
;    :config
;    (which-key-mode))

;; Custom #######################################################################

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(lsp-ivy lsp-ui lsp-mode ivy-rich counsel ivy elixir-mode visual-fill-column emmet-mode evil-numbers evil-surround evil-commentary evil-collection evil doom-themes doom-modeline use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
