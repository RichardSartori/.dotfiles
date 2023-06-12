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
(global-display-line-numbers-mode)
(setq column-number-mode t)

;; show matching parenthesis
(show-paren-mode 1)

;; automatic reload when file changed on disk
(global-auto-revert-mode)

;; tab behavior
(setq-default tab-width 4)
(setq-default tab-stop-list nil)
(global-set-key (kbd "TAB") 'tab-to-tab-stop)
(setq indent-tabs-mode t)
(setq electric-indent-mode nil)

;; show tabs as gray pipe
(setq whitespace-display-mappings '((tab-mark 9 [124 9] [92 9])))
(custom-set-faces '(whitespace-tab ((t (:foreground "#636363")))))

;; delete tabs at once
(setq backward-delete-char-untabify-method nil)

;; fill-paragraph behavior
(defconst max-column 80)
(set-fill-column max-column)

;; show trailing spaces/tabs
(setq whitespace-style '(face tabs tab-mark trailing))

;; whitespace mode everywhere
(global-whitespace-mode)

;; location of custom scripts (in the same directory as this file)
(add-to-list 'load-path
	(file-name-directory (file-truename load-file-name)))
(add-to-list 'custom-theme-load-path
	(file-name-directory (file-truename load-file-name)))

;; syntax highlighting for LLVM
;; https://github.com/llvm-mirror/llvm/blob/master/utils/emacs/llvm-mode.el
(require 'llvm-mode)

;; sublime-text looking theme
;; https://github.com/oneKelvinSmith/monokai-emacs/blob/master/monokai-theme.el
(load-theme 'monokai t)

;; location of auto-saves
(setq backup-directory-alist '(("." . "~/.trash")))
(setq backup-by-copying t)

;; C-x f auto-complete
(ido-mode 1)

;; key bindings
(global-set-key (kbd "C-v") 'yank); paste
(global-set-key (kbd "C-f") 'isearch-forward); find
(define-key isearch-mode-map (kbd "<f3>") 'isearch-repeat-forward)
(define-key isearch-mode-map (kbd "S-<f3>") 'isearch-repeat-backward)
(global-set-key (kbd "C-s") 'save-buffer); save
(global-set-key (kbd "C-w") 'save-buffers-kill-terminal); quit

; TODO: other bindings, use C-h b to show emacs bindings
; TODO: try to keep usual OS bindings
; TODO: find alternatives for prefix commands
;(global-set-key (kbd "C-c") 'kill-ring-save); copy FIXME: C-c is prefix
;(global-set-key (kbd "C-x") 'kill-region); cut FIXME: C-x is prefix
;(global-set-key (kbd "<C-TAB>") '<C-x o>) FIXME: how to map <C-TAB> to <C-x o>
;(global-set-key (kbd "C-z") 'undo) FIXME: C-z bring back to the terminal
;(global-set-key (kbd "C-y") 'redo) FIXME: 'redo is not a function

;; update packages :
;; `M-x list-packages`
;; `C-s installed` and find installed packages
;; type U (uppercase u) on the left of the package you want to upgrade
;; type x to apply upgrades
;; type q to quit

;; ============================================ ;;
;; commands below require non built-in packages ;;
;; ============================================ ;;
(defconst with-external t) ; set to nil if unable to install packages
(when with-external

	;; M-x auto-complete
	(require 'smex)
	(global-set-key (kbd "M-x") 'smex)

	;; syntax highlighting for Rust
	(require 'rust-mode)

	;; general auto-completion
	(require 'auto-complete)
	(ac-config-default)
	(add-to-list 'ac-modes 'rust-mode)

	;; multiple cursors
	(require 'multiple-cursors)
	(global-set-key (kbd "<M-down>") 'mc/mark-next-like-this)
	(global-set-key (kbd "<M-up>") 'mc/mark-previous-like-this)
	(global-set-key (kbd "<M-right>") 'mc/mark-next-like-this)
	(global-set-key (kbd "<M-left>") 'mc/mark-previous-like-this)

	;; highlight keywords
	(require 'hl-todo)
	(setq hl-todo-keyword-faces '(
		("TODO"  . "#00FF00")
		("FIXME" . "#00FF00")
		("TMP"   . "#00FF00")))
	(global-hl-todo-mode)

	;; highlight text beyond max-column
	(require 'column-enforce-mode)
	(setq column-enforce-column max-column)
	(defface black-on-red `((t (
		:inherit font-lock-warning-face
		:background "red")))
		"custom face for column-enforce-face"
		:group 'column-enforce)
	(setq column-enforce-face 'black-on-red)
	(global-column-enforce-mode)
	(setq column-enforce-should-enable-p '(lambda ()
		(derived-mode-p 'prog-mode 'tex-mode)))
)
