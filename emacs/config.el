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
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode)
(setq column-number-mode t)

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

;; location of custom scripts (in the same directory as this file)
(add-to-list 'load-path              (file-name-directory (file-truename load-file-name)))
(add-to-list 'custom-theme-load-path (file-name-directory (file-truename load-file-name)))

;; syntax highlighting for LLVM
;; source: https://github.com/llvm-mirror/llvm/blob/master/utils/emacs/llvm-mode.el
(require 'llvm-mode)

;; sublime-text looking theme
;; source: https://github.com/oneKelvinSmith/monokai-emacs/blob/master/monokai-theme.el
(load-theme 'monokai t)

;; location of auto-saves
(setq backup-directory-alist '(("." . "~/.trash")))
(setq backup-by-copying t)

;; C-x f auto-complete
(ido-mode 1)

;; ============================================ ;;
;; commands below require non built-in packages ;;
;; ============================================ ;;

;; update packages :
;; `M-x list-packages`
;; `C-s installed` and find installed packages
;; type U (uppercase u) on the left of the package you want to upgrade
;; type x to apply upgrades

;; M-x auto-complete
(require 'smex)
(global-set-key (kbd "M-x") 'smex)

;; general auto-complete
(require 'auto-complete)
(ac-config-default)

;; multiple cursors
(require 'multiple-cursors)
(global-set-key (kbd "<M-down>") 'mc/mark-next-like-this)
(global-set-key (kbd "<M-up>") 'mc/mark-previous-like-this)

;; highlight keywords
(require 'hl-todo)
(setq hl-todo-keyword-faces '(
	("TODO"  . "#00FF00")
	("FIXME" . "#00FF00")
	("TMP"   . "#00FF00")))
(global-hl-todo-mode)
