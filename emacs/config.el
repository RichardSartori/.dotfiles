;; initialize packages
(package-initialize)

;; MELPA
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; no startup screen
(setq inhibit-startup-screen t)

;; set side bars
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)

;; show matching parenthesis
(show-paren-mode 1)

;; tab with
(setq-default tab-width 4)

;; tab behavior
(defun enable-tabs ()
	;; TAB to next tab-stop
	(local-set-key (kbd "TAB") 'tab-to-tab-stop)
	;; indent with tabs
	(setq indent-tabs-mode t)
	;; do no indent automatically
	(setq electric-indent-mode nil)
)

;; execute 'enable-tabs in prog env
(add-hook 'prog-mode-hook 'enable-tabs)

;; delete tabs at once
(setq backward-delete-char-untabify-method 'hungry)

;; show trailing spaces/tabs
(setq whitespace-style '(face tabs tab-mark trailing))

;; show tabs as pipe
(setq whitespace-display-mappings '((tab-mark 9 [124 9] [92 9])))

;; color of tab symb
(custom-set-faces '(whitespace-tab ((t (:foreground "#636363")))))

;; whitespace mode everywhere
(global-whitespace-mode)

;; location of custom themes
(add-to-list 'custom-theme-load-path "~/.config_files/emacs/")

;; sublime-text looking theme
(load-theme 'monokai t)

;; location of auto-saves
(setq backup-directory-alist '(("." . "~/.trash")))
(setq backup-by-copying t)

;; auto-complete
(ido-mode 1)
(global-set-key (kbd "M-x") 'smex)
