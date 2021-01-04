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
    blacken                         ;; Black formatting on save
    ein                             ;; Emacs IPython Notebook
    go-mode                         ;; Go programming language support
    fsharp-mode                     ;; F# mode
    dotnet                          ;; .NET CLI
    julia-mode                      ;; Julia language
    julia-repl                      ;; Julia REPL
    flycheck                        ;; On the fly syntax checking
    flycheck-julia                  ;; Flycheck support for Julia
    material-theme                  ;; Theme
    solarized-theme                 ;; Theme
    nord-theme                      ;; Theme
    eglot                           ;; eglot LSP
    tablist                         ;; pre-req for pdf-tools
    pdf-tools                       ;; Support library for PDF documents
    magit                           ;; Git
    company                         ;; Autocompletion
    company-lsp                     ;; Autocompletion
    company-box                     ;; Icons for company
    use-package                     ;; Package configuration
    multiple-cursors                ;; Multiple cursor functionality
    which-key                       ;; Key chord completions and suggestions
    all-the-icons                   ;; Icons
    all-the-icons-dired             ;; Dired support for all-the-icons
    dired-subtree                   ;; Working with subdirectories
    dired-sidebar                   ;; Dired sidebar window
    paredit                         ;; TODO set this up
    vterm                           ;; Terminal emulator
    dimmer                          ;; Visually highlight the selected buffer
    neotree                         ;; tree style sidebar
    projectile                      ;; Project interaction library
    exec-path-from-shell            ;; Inherit $PATH from shell configuration
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

(find-file "~/list.org")

(defun open-init-file ()
  "Open this very file."
  (interactive)
  (find-file "~/.emacs.d/init.el"))
(bind-key "C-c e" #'open-init-file)

;;;;Org mode configuration
;; Enable Org mode
(require 'org)

;;;; Terminal
;; Set default shell

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(setq-default explicit-shell-file-name "bash")

(set-face-attribute 'comint-highlight-prompt nil
                    :inherit nil)

;;(use-package vterm
;;    :ensure t)

(shell)

;; Load any files
(add-to-list 'load-path "~/.emacs.d/lisp/")

(setq inhibit-startup-message t)      ;; Hide the startup message

;; Show matching parenthesis
(show-paren-mode 1)

(delete-selection-mode t)

;; Load theme
(load-theme 'nord t)

;; Set default font
;;(set-frame-font "JetBrains Mono Regular 11" nil t)

(if ( version< "27.0" emacs-version ) ; )
    (set-fontset-font "fontset-default" 'unicode "Apple Color Emoji" nil 'prepend)
  (warn "This Emacs version is too old to properly support emoji."))

(use-package all-the-icons)
(use-package all-the-icons-dired
  :after all-the-icons
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package dired-sidebar
  :bind (("C-c n" . dired-sidebar-toggle-with-current-directory))
  :ensure t
  :commands (dired-sidebar-toggle-sidebar))

(require 'neotree)
(global-set-key [f8] 'neotree-toggle)

(use-package multiple-cursors
  :bind (("C-c m m" . #'mc/edit-lines )
         ("C-c m d" . #'mc/mark-all-dwim )))

;; (global-linum-mode t)                ;; Older method
(global-display-line-numbers-mode t) ;; Enable line numbers globally, is this compatible with pdf-tools
(column-number-mode 1)               ;; show cursor position within line
(tool-bar-mode -1)
(toggle-scroll-bar -1)

;;(when (not (window-system))
(menu-bar-mode -1)
;;)

;; TODO: Use relative line numbers?
;;(setq linum-format "%4d \u2502 ")
(setq-default left-fringe-width  5)

;; Set initial window size
;;(add-to-list 'default-frame-alist '(fullscreen . maximized))

(defconst frame-default-top      0  "The 'top'  position property of a frame.")
(defconst frame-default-left  1708  "The 'left' position property of a frame.")
(defconst frame-default-height 100  "The default frame height.")
(defconst frame-default-width  119  "The default frame width.")

(add-to-list 'default-frame-alist (cons 'left   frame-default-left))
(add-to-list 'default-frame-alist (cons 'top    frame-default-top))
(add-to-list 'default-frame-alist (cons 'height frame-default-height))
(add-to-list 'default-frame-alist (cons 'width  frame-default-width))

;; Set Frame width/height
;;  (setq default-frame-alist
;;        '((top . 25) (left . 275) (width . 105) (height . 60)))
;; (when (version<= "26.0.50" emacs-version )
;;   (global-display-line-numbers-mode)) ;; Off for pdf-tools

(require 'recentf)           ;; Keep a list of recently opened files
(recentf-mode 1)
(add-to-list 'recentf-exclude "\\elpa")
(add-to-list 'recentf-exclude "\\ido.last")
(add-to-list 'recentf-exclude "\\recentf")
(add-to-list 'recentf-exclude "\\init.el")

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
(global-set-key (kbd "C-}") `buffer-stack-up)
(global-set-key [mouse-5]   `buffer-stack-up)
(global-set-key (kbd "C-{") `buffer-stack-down)
(global-set-key [mouse-4]   `buffer-stack-down)

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
(global-set-key (kbd "M-[")   'move-line-up)
(global-set-key (kbd "<M-down>") 'move-line-down)
(global-set-key (kbd "M-]") 'move-line-down)

;; Use elpy's Shift+return functionality globally
(global-set-key (kbd "<S-return>") 'elpy-open-and-indent-line-below)

(use-package which-key
  :custom
  (which-key-setup-side-window-bottom)
  (which-key-enable-extended-define-key t)
  :config
  (which-key-setup-minibuffer))

(defun copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode) default-directory (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

(bind-key "C-c P" #'copy-file-name-to-clipboard)

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

;; F# mode
(require 'fsharp-mode)
(require 'eglot-fsharp)
;; (setq inferior-fsharp-program "/usr/bin/dotnet fsi --readline-")
(setq inferior-fsharp-program "dotnet fsi")
;;(setq inferior-fsharp-program "c:\\program files\\dotnet\\sdk\\5.0.100\\fsharp\\fsi.exe")
(setq-default fsharp-indent-offset 2)
(add-hook 'fsharp-mode-hook 'highlight-indentation-mode)
(add-hook 'fsharp-mode-hook 'dotnet-mode)

;;(require 'lsp-mode)
;;(add-hook 'fsharp-mode-hook #'lsp)

(use-package lsp-mode
    :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
            (fsharp-mode . lsp)
            ;; if you want which-key integration
            (lsp-mode . lsp-enable-which-key-integration))
    :commands lsp)

;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
(setq lsp-keymap-prefix "C-.")

;; optionally
(use-package lsp-ui :commands lsp-ui-mode)
;; if you are helm user
;;(use-package helm-lsp :commands helm-lsp-workspace-symbol)
;; if you are ivy user
;;(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
;;(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

;; optionally if you want to use debugger
;;(use-package dap-mode)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language

;; optional if you want which-key integration
(use-package which-key
    :config
    (which-key-mode))

;; Flycheck
(use-package flycheck
	     :ensure t)

;; Company
(use-package company
  :defer 2
  :diminish
  :custom
  (company-begin-commands '(self-insert-command))
  (company-idle-delay .1)
  (company-minimum-prefix-length 2)
  (company-show-numbers t)
  (company-tooltip-align-annotations 't)
  (global-company-mode t))

(use-package company-box
  :after company
  :diminish
  :hook (company-mode . company-box-mode))


(require 'julia-mode)

(require 'flycheck-julia)
;;(flycheck-julia-setup)
;;(add-to-list 'flycheck-global-modes 'julia-mode)
(require 'julia-repl)
(add-hook 'julia-mode-hook 'julia-repl-mode)
