(package-initialize)

;; MELPA
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

;; no startup screen
(setq inhibit-startup-screen t)

;; set side bars
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(setq column-number-mode t)
(when (version<= "26.0.50" emacs-version)
	(global-display-line-numbers-mode))

;; visible cursor
(blink-cursor-mode 0)
(setq visible-cursor nil)
(global-hl-line-mode)

;; show matching parenthesis
(show-paren-mode 1)

;; automatic reload when file changed on disk
(global-auto-revert-mode)

;; my preferences
(defconst my-max-column 80)
(defconst my-tab-width 4)
(defconst highlight-color "#00FF00")
(setq-default fill-column my-max-column)
(setq-default tab-width my-tab-width)
(setq-default tab-stop-list nil)
(global-set-key (kbd "TAB") 'tab-to-tab-stop)
(fset 'yes-or-no-p 'y-or-n-p)

;; preferences set in every buffer
(add-hook 'after-change-major-mode-hook (lambda ()
	(setq indent-tabs-mode t)
	(setq electric-indent-mode nil)
	(when (derived-mode-p 'python-mode)
		(setq python-indent my-tab-width)
		(setq tab-width my-tab-width))))

;; show tabs as gray pipe
(setq whitespace-display-mappings '((tab-mark 9 [124 9] [92 9])))
(custom-set-faces '(whitespace-tab ((t (:foreground "#636363")))))

;; delete tabs at once
(setq backward-delete-char-untabify-method nil)

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
;; https://github.com/llvm/llvm-project/blob/main/llvm/utils/emacs/llvm-mode.el
(require 'llvm-mode)

;; sublime-text looking theme
;; https://github.com/oneKelvinSmith/monokai-emacs/blob/master/monokai-theme.el
(load-theme 'monokai t)

;; location of auto-saves
(setq backup-directory-alist '(("." . "~/.trash")))
(setq backup-by-copying t)

;; C-x f auto-complete
(ido-mode 1)

;; update installed packages
; fallback method: `M-x list-packages RET` `S-u` `y` `x` `q`
(defun update-packages ()
	"Update and upgrade all installed Emacs packages"
	(interactive)
	(require 'package)
	(package-initialize)
	(package-refresh-contents)
	(dolist (pkg package-alist)
		(let* ((name (car pkg))
			(installed (package-desc-version (cadr pkg)))
			(available (when (assoc name package-archive-contents)
				(package-desc-version (cadr (assoc name package-archive-contents))))))
			(when (and available (version-list-< installed available)
				(package-reinstall name))))))

;; ============= ;;
;; key bindings  ;;
;; ============= ;;

;; TODO: fix bindings:
; unmap most (prefix) bindings
; see (setq esc-map (make-sparse-keymap))
; see (setq ctl-x-map (make-sparse-keymap))
; add only the bindings I use
; try to keep usual OS bindings (ie. C-c = copy, ...)

;; NOTES:
; use C-h b to show emacs bindings
; use C-h k <keys> to see what function <keys> run
; run (loop (key func) (define-key local-map key func)) in hooks
; (electric-indent-mode nil) *activates* the minor mode
; see /usr/share/emacs/{VERSION}/lisp for bindings/functions
; see https://robert.kra.hn/posts/rust-emacs-setup/

;; usual OS bindings, mapped with C-
(global-set-key (kbd "C-v") 'yank); paste
(global-set-key (kbd "C-f") 'isearch-forward); find
(define-key isearch-mode-map (kbd "<f3>") 'isearch-repeat-forward)
(define-key isearch-mode-map (kbd "S-<f3>") 'isearch-repeat-backward)
(global-set-key (kbd "C-s") 'save-buffer); save
(global-set-key (kbd "C-a") 'mark-whole-buffer); select-all

;; unusual bindings, mapped with M-
(global-set-key (kbd "M-c") 'comment-dwim); toggle comment/uncomment region
(global-set-key (kbd "M-m") 'recenter-top-bottom); put cursor at the middle
(global-set-key (kbd "M-f") 'fill-paragraph); add newlines before my-max-column
(global-set-key (kbd "M-q") 'keyboard-quit); cancel operation
(global-set-key (kbd "M-v") 'view-mode); set buffer read-onlyw
(global-set-key (kbd "M-s") 'suspend-emacs); bring back to the terminal

;; sync kill-ring with clipboard
(defun copy-to-clipboard (begin end)
	(interactive "r")
	(if (region-active-p)
		(progn
			(call-process-region begin end
				"xclip" nil nil nil "-selection" "clipboard" "-i")
			(message "Copied to clipboard")
			(deactivate-mark))
	(message "No region selected")))
(defun cut-to-clipboard (begin end)
	(interactive "r")
	(if (region-active-p)
		(progn
			(copy-to-clipboard begin end)
			(kill-region begin end))))
(defun paste-from-clipboard ()
	(interactive)
	(insert (shell-command-to-string "xclip -selection clipboard -o")))
(global-set-key (kbd "M-w") 'copy-to-clipboard)
(global-set-key (kbd "C-w") 'cut-to-clipboard)
(global-set-key (kbd "C-y") 'paste-from-clipboard)

;; toggle hide/show code block
(require 'hideshow)
(defun hidden-block-appearance (ov)
	"Custom appearance of hidden blocks"
	(when (eq 'code (overlay-get ov 'hs))
		(overlay-put ov 'display
			(propertize " ••• " 'face `(
				:foreground ,highlight-color
				:weight bold)))))
(add-hook 'prog-mode-hook (lambda ()
	(hs-minor-mode)
	(setq hs-set-up-overlay 'hidden-block-appearance)
	(local-unset-key (kbd "C-M-h"))
	(local-set-key (kbd "C-M-h") 'hs-toggle-hiding)))

;; TODO: map these bindings:
;(global-set-key (kbd "C-c") 'kill-ring-save); copy FIXME: C-c is prefix
;(global-set-key (kbd "C-x") 'kill-region); cut FIXME: C-x is prefix
;(global-set-key (kbd "<C-TAB>") '<C-x o>) FIXME: how to map <C-TAB> to <C-x o>
;(global-set-key (kbd "C-z") 'undo) FIXME: C-z bring back to the terminal
;(global-set-key (kbd "C-y") 'redo) FIXME: 'redo is not a function
;(global-set-key (kbd "C-w") 'save-buffers-kill-terminal); quit FIXME: is cut
;(global-set-key (kbd "C-0") 'delete-window) FIXME: does not work
;(global-set-key (kbd "C-1") 'delete-other-windows) FIXME: does not work
;(global-set-key (kbd "C-2") 'split-window-below) FIXME: does not work
;(global-set-key (kbd "C-3") 'split-window-right) FIXME: does not work

;; TODO: find better bindings for:
;(global-set-key (kbd "C-M-<down>") #'down-list); move down a code block
;(global-set-key (kbd "C-M-<up>") #'backward-up-list); move up a code block
;(global-set-key (kbd "C-M-<right>") #'down-list); move down a code block
;(global-set-key (kbd "C-M-<left>") #'backward-up-list); move up a code block

;; ============================================ ;;
;; commands below require non built-in packages ;;
;; ============================================ ;;

;; M-x auto-complete
(when (require 'smex nil t)
	(global-set-key (kbd "M-x") 'smex))

;; syntax highlighting for Rust
(require 'rust-mode nil t)

;; language server protocol (LSP)
(defconst lsp-modes
	(list 'c-mode-hook 'c++-mode-hook 'rust-mode-hook 'python-mode-hook))
(when (and (require 'lsp-mode nil t) (require 'company nil t))
	(setq
		lsp-enable-snippet nil
		lsp-prefer-capf t ; completions at point
		lsp-diagnostics-provider :none ; disable on-the-fly error checks
		lsp-auto-guess-root t
		lsp-restart 'ignore ; don't prompt
		lsp-enable-file-watchers nil ; performance
		lsp-format-on-save nil
		lsp-before-save-edits nil
		lsp-enable-indentation nil
		lsp-enable-formatting nil
		lsp-apply-edits nil ; don't even think about touching my code
		lsp-completion-item-limit 5
		lsp-enable-on-type-formatting nil
		lsp-enable-text-document-sync t
		lsp-signature-auto-activate t
		lsp-completion-enable-additional-text-edit nil
		company-auto-expand nil
		company-auto-complete nil
		company-auto-complete-chars nil
		company-auto-commit nil
		company-auto-commit-chars nil
		company-backends '((company-lsp)))
	(global-company-mode)
	(dolist (hook lsp-modes)
		(add-hook hook #'lsp))
	;(global-set-key (kbd "<tab>") #'company-indent-or-complete-common)
	;(define-key company-active-map (kbd "TAB") #'company-complete)
	(define-key company-active-map (kbd "TAB") #'company-complete-selection)
	(define-key company-active-map (kbd "RET") nil)
	;(with-eval-after-load 'lsp-mode (setq lsp-client-settings '(
	;	:textDocument/formatting nil
	;	:textDocument/rangeFormatting nil
	;	:textDocument/onTypeFormatting nil
	;	:textDocument/documentHighlight nil)))
)

;; TODO:
; check for all the options: what they do, are they required
; limit completions, strict matching, make TAB work
; add lsp-ui?

;; general auto-completion
(when (require 'auto-complete nil t)
	(ac-config-default)
	(dolist (hook lsp-modes)
		(add-hook hook (lambda ()
			(auto-complete-mode -1)))))

;; multiple cursors
(when (require 'multiple-cursors nil t)
	(global-set-key (kbd "<M-down>") 'mc/mark-next-like-this)
	(global-set-key (kbd "<M-up>") 'mc/mark-previous-like-this)
	(global-set-key (kbd "<M-right>") 'mc/mark-next-like-this)
	(global-set-key (kbd "<M-left>") 'mc/mark-previous-like-this))

;; highlight keywords
(when (require 'hl-todo nil t)
	(setq hl-todo-keyword-faces `(
		("TODO"  . ,highlight-color)
		("FIXME" . ,highlight-color)
		("TMP"   . ,highlight-color)))
	(global-hl-todo-mode))

;; highlight text beyond my-max-column
(when (require 'column-enforce-mode nil t)
	(setq column-enforce-column my-max-column)
	(defface black-on-red `((t (
		:inherit font-lock-warning-face
		:background "red")))
		"custom face for column-enforce-face"
		:group 'column-enforce)
	(setq column-enforce-face 'black-on-red)
	(global-column-enforce-mode)
	(setq column-enforce-should-enable-p '(lambda ()
		(derived-mode-p 'prog-mode 'tex-mode))))

;; keep current function/class/namespace visible
(when (require 'stickyfunc-enhance nil t)
	(add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)
	(semantic-mode 1))
