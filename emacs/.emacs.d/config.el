(setq user-full-name "Dustin Lyons"
  user-mail-address "hello@dustinlyons.co")

;; Turn off the splash screen
(setq inhibit-startup-screen t)

;; CUSTOM FUNCTIONS, we load this file at the very end
(setq custom-file "~/.emacs.d/local-config.org")
(load custom-file t)

(unless (assoc-default "melpa" package-archives)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))
(unless (assoc-default "nongnu" package-archives)
  (add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/") t))

;; use-package package provides common package import functions
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(setq use-package-verbose t)
(setq use-package-always-ensure t)
(require 'use-package)

;; This sets up straight.el, a git package manager
(defvar bootstrap-version)
  (let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
	 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Tells (use-package) to use straight.el to download packages
;; straight.el uses git packages, instead of the default bin files, which we like
(setq straight-use-package-by-default t)

(use-package doom-themes
  :init (load-theme 'doom-dracula t))

(setq use-dialog-box nil
    use-file-dialog nil
    cursor-type 'bar)

(set-face-attribute 'default nil :font "Hack" :height 100)

(global-linum-mode 1)
(defvar my-linum-current-line-number 0)
(setq linum-format 'my-linum-relative-line-numbers)

;; This fancy function handles the math behind relative line numbers
(defun my-linum-relative-line-numbers (line-number)
(let ((y (1+ (- line-number my-linum-current-line-number))))
    (propertize
    (number-to-string
	(cond ((<= y 0) (abs (- y 2))) ((> y 0) y)))
	'face 'linum)))

(defadvice linum-update (around my-linum-update)
(let ((my-linum-current-line-number (line-number-at-pos)))
    ad-do-it))

;; Turn it on
(ad-activate 'linum-update)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
		term-mode-hook
		shell-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (linum-mode 0))))

;; Turn off UI junk
(column-number-mode)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Run M-x all-the-icons-install-fonts to install
(use-package all-the-icons)
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

(global-visual-line-mode t) ;; Wraps lines everywhere
(line-number-mode t) ;; Line numbers in the gutter
(show-paren-mode t) ;; Highlights parans for me

(use-package org
  :defer t
  :config
    (setq org-edit-src-content-indentation 2
          org-hide-block-startup nil))

(defun dl/evil-hook ()
  (dolist (mode '(eshell-mode
		  git-rebase-mode
		  term-mode))
  (add-to-list 'evil-emacs-state-modes mode))) ;; no evil mode for these modes

(use-package evil
  :init
  (setq evil-want-integration t) ;; TODO: research what this does
  (setq evil-want-keybinding nil)
  (setq evil-want-fine-undo 'fine) ;; undo/redo each motion 
  (setq evil-want-Y-yank-to-eol t) ;; Y copies to end of line like vim
  (setq evil-want-C-u-scroll t) ;; vim like scroll up
  :hook (evil-mode . dl/evil-hook)
  :config
  (evil-mode 1)

  ;; Emacs "cancel" == vim "cancel"
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  ;; Ctrl-h deletes in vim insert mode
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)
  ;; When we wrap lines, jump visually, not to the "actual" next line
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'message-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

;; Gives me vim bindings elsewhere in emacs
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; Keybindings in org mode
(use-package evil-org
  :after org
  :hook (org-mode . (lambda () evil-org-mode))
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

;; Branching undo system
(use-package undo-tree
  :after evil
  :diminish 
  :config
  (evil-set-undo-system 'undo-tree)
  (global-undo-tree-mode 1))

(use-package general
  :config
  (general-create-definer dl/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix ",")

  (dl/leader-keys
    "tt" '(counsel-load-theme :which-key "choose theme")
    "<escape>" '(keyboard-escape-quit :which-key "quit")
    "C-M-j" '(counsel-switch-buffer :which-key "switch buffer")))

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-f" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :init
  (ivy-mode 1)
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-wrap t)
  (setq ivy-count-format "(%d/%d) ")
  (setq enable-recursive-minibuffers t))

(use-package counsel
  :demand t
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         ("C-M-j" . counsel-switch-buffer)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :custom
  (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
  :config
  (setq ivy-initial-inputs-alist nil)) ;; Don't start searches with ^

;; This uses Github Flavored Markdown for README files
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
    ("\\.md\\'" . markdown-mode)
    ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(setq org-src-tab-acts-natively t)

;; Gives me a fancy list of commands I run
(use-package command-log-mode)
(setq global-command-log-mode t)
;; TODO Install package that lets you define help screens for keymaps

;; Gives me a fancy list of commands I run
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package helpful
  :custom
  ;; Remap Counsel help functions
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
    :bind
  ;; Remap default help functions
  ([remap describe-function] . helpful-function)
  ([remap describe-symbol] . helpful-symbol)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-command] . helpful-command)
  ([remap describe-key] . helpful-key))
