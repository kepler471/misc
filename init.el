;; .emacs.d/init.el
;; ===================================
;; MELPA Package Support
;; ===================================
;; Enables basic packaging support
(require 'package)

;; Adds the Melpa archive to the list of available repositories
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

;; Initializes the package infrastructure
(package-initialize)

;; If there are no archived package contents, refresh them
(when (not package-archive-contents)
  (package-refresh-contents))

;; Installs packages
;;
;; myPackages contains a list of package names
(defvar myPackages
  '(better-defaults                 ;; Set up some better Emacs defaults
    elpy                            ;; Emacs Lisp Python Environment
    flycheck                        ;; On the fly syntax checking
    blacken                         ;; Black formatting on save
    ein                             ;; Emacs IPython Notebook
    material-theme                  ;; Theme
    solarized-theme                 ;; Theme
    fsharp-mode                     ;; F# mode
    tablist                         ;; pre-req for pdf-tools
    pdf-tools                       ;; Support library for PDF documents
    go-mode                         ;; Go programming language support
    magit                           ;; Git
    compant                         ;; Autocompletion
    company-lsp                     ;; Autocompletion
    use-package                     ;; Package configuration
    )
  )

;; Scans the list in myPackages
;; If the package listed is not already installed, install it
(mapc #'(lambda (package)
          (unless (package-installed-p package)
            (package-install package)))
      myPackages)

;; ===================================
;; Basic Customization
;; ===================================
;; Load any files
(add-to-list 'load-path "~/.emacs.d/lisp/")

(setq inhibit-startup-message t)      ;; Hide the startup message

;;(load-theme 'material t)            ;; Load material theme
(load-theme 'solarized-light t)        ;; Load material theme

;; Global linum mode off for pdf-tools compatibility
(global-linum-mode t)               ;; Enable line numbers globally
(column-number-mode 1)              ;; show cursor position within line
(tool-bar-mode -1)

;; (when (version<= "26.0.50" emacs-version )
;;   (global-display-line-numbers-mode)) ;; Off for pdf-tools

(require 'recentf)           ;; Keep a list of recently opened files
(recentf-mode 1)

;; Use ido-mode for find buffer and find file suggestions
(require 'ido)
(ido-mode t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(lsp-ui flycheck-golangci-lint magit go-mode fsharp-mode material-theme better-defaults)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Keybindings

;; Move between windows
(global-set-key (kbd "<M-S-up>")    'windmove-up)
(global-set-key (kbd "<M-S-down>")  'windmove-down)
(global-set-key (kbd "<M-S-left>")  'windmove-left)
(global-set-key (kbd "<M-S-right>") 'windmove-right)

(defun goto-previous-window()
  "Move to the previous window"
  (interactive)
  (select-window (previous-window)))

(global-set-key (kbd "C-x p") 'goto-previous-window)

(defun goto-last-window()
  "Go to the last used window"
  (interactive)
  (other-window -1))

(global-set-key (kbd "<C-tab>") 'goto-last-window)

(global-set-key (kbd "C-#") (lambda()
			      (interactive)
			      (ido-switch-buffer)))
			      ;(tab))) ;; how to use a keysequence here, to press enter or tab? use the first suggestion by ido?

;; Load buffer-stack for tab buffer switching
(load "buffer-stack")
(require 'buffer-stack)
(global-set-key (kbd "C-{") `buffer-stack-up)
(global-set-key [mouse-8]   `buffer-stack-up)
(global-set-key (kbd "C-}") `buffer-stack-down)
(global-set-key [mouse-9]   `buffer-stack-down)

(global-set-key (kbd "C-x  r") `recentf-open-most-recent-file) ;; Open last open file

(defun duplicate-line()
  "Duplicate a line"
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (yank)
  (open-line 1)
  (next-line 1)
  (yank))

;; Duplicate line
(global-set-key (kbd "C-c C-d") `duplicate-line)

;; Move lines up and down
(defun move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defun move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

(global-set-key (kbd "<M-up>")   'move-line-up)
(global-set-key (kbd "<M-down>") 'move-line-down)

;; Use elpy's Shift+return functionality globally
(global-set-key (kbd "<S-return>") 'elpy-open-and-indent-line-below)

;; ====================================
;; Development Setup
;; ====================================
;; Enable pdf-tools
(pdf-loader-install)

;; Enable elpy
(elpy-enable)
;; Use IPython for REPL
(setq python-shell-interpreter "jupyter"
      python-shell-interpreter-args "console --simple-prompt"
      python-shell-prompt-detect-failure-warning nil)
(add-to-list 'python-shell-completion-native-disabled-interpreters
             "jupyter")

;; Enable Flycheck
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

(require 'fsharp-mode)
(require 'eglot-fsharp)
(setq inferior-fsharp-program "/usr/bin/dotnet fsi --readline-")
(setq-default fsharp-indent-offset 2)
(add-hook 'fsharp-mode-hook 'highlight-indentation-mode)

;; Go mode
(use-package go-mode
  :mode "\\.go\\'"
  :custom (gofmt-command "goimports")
  :bind (:map go-mode-map
         ("C-c C-n" . go-run)
         ("C-c ."   . go-test-current-test)
         ("C-c f"   . go-test-current-file)
         ("C-c a"   . go-test-current-project))
  :config
  (add-hook 'before-save-hook #'gofmt-before-save)
  (use-package gotest)
  (use-package go-tag
    :config (setq go-tag-args (list "-transform" "camelcase"))))

(use-package flycheck
	     :ensure t)

(use-package flycheck-golangci-lint
	     :ensure t)

;; Optional - provides snippet support.
(use-package yasnippet
  :ensure t
  :commands yas-minor-mode
  :hook (go-mode . yas-minor-mode))

(use-package lsp-mode
	     :ensure t
	     ;;:custom (lsp-gopls-server-args '("debug" "127.0.0.1:0"))
	     :commands (lsp lsp-deferred)
	     :hook (go-mode . lsp-deferred))
;	     config (progn
;		      (setq lsp-prefer-flymake nil) ;; use flycheck over flymake
;		      ;;(setq lsp-trace nil)
;		      (setq lsp-print-performance nil) ;; 
;		      (setq lsp-log-io nil)))

;; Optional - provides fancier overlays.
(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)
  ;;:config (progn
 ;;           ;; disable inline documentation
 ;;           (setq lsp-ui-sideline-enable nil)
 ;;           ;; disable showing docs on hover at the top of the window
 ;;           (setq lsp-ui-doc-enable nil)
;;	    (setq lsp-ui-imenu-enable t)
;;	    (setq lsp-ui-imenu-kind-position 'top))

;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

;; Company mode is a standard completion package that works well with lsp-mode.
(use-package company
  :ensure t
  :config
  ;; Optionally enable completion-as-you-type behavior.
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1))
  ;;(setq company-tooltip-align-annotations t)

;(require 'company-lsp)
;(push 'company-lsp company-backends)

;; https://arenzana.org/2019/01/emacs-go-mode/

;; User-Defined init.el ends here

