;; Set theme
(load-theme 'doom-tokyo-night t)

;; Set transparency
(set-frame-parameter (selected-frame) 'alpha '(94 94))

;; Set font family/size
(set-frame-font "FiraCode Nerd Font Mono 12" nil t)

;; Color Column/Ruler
(global-display-fill-column-indicator-mode t)
(setq-default display-fill-column-indicator-column 101)

;; Relative line numbers
(global-display-line-numbers-mode t)
(setq-default display-line-numbers-type 'relative)
(set-face-foreground 'line-number "#858565")

;; Tab to 4 spaces
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)

(when (featurep! :lang csharp +lsp))
