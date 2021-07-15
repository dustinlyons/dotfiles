;; Turn off the splash screen
(setq inhibit-startup-screen t)

;; Custom functions, we load this file at the very end
(setq custom-file "~/.emacs.d/local-config.org")
(load custom-file t)

(setq user-full-name "Dustin Lyons"
  user-mail-address "hello@dustinlyons.co")

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

(use-package evil
  :init
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))

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
  :diminish undo-tree-mode
  :config
  (evil-set-undo-system 'undo-tree)
  (global-undo-tree-mode 1))

;; Undo/redo each motion, emulates vim behavior
(setq evil-want-fine-undo 'fine)

(display-time-mode t)
(line-number-mode t)
(show-paren-mode t)

;; This uses Github Flavored Markdown for README files
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
	 ("\\.md\\'" . markdown-mode)
	 ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(add-to-list 'custom-theme-load-path "~/.dotfiles/emacs/.emacs.d/themes")
(load-theme 'dracula t)

(setq use-dialog-box nil
    use-file-dialog nil
    cursor-type 'bar)

;; Turn off UI junk
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
