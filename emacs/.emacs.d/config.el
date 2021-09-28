(setq user-full-name "Dustin Lyons"
  user-mail-address "hello@dustinlyons.co")

;; Turn off the splash screen
(setq inhibit-startup-screen t)

(unless (assoc-default "melpa" package-archives)
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))
(unless (assoc-default "org" package-archives)
  (add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t))
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
;; as it's much easier to open it up and hack it
(setq straight-use-package-by-default t)

(use-package dashboard
  :config
  (dashboard-setup-startup-hook))

  (setq dashboard-projects-backend 'projectile)
  (setq dashboard-items '((projects . 5)
                          (bookmarks . 5)
                          (agenda . 5)))

  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-filter-agenda-entry 'dashboard-no-filter-agenda)

;; ESC will also cancel/quit/etc.
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(use-package general
  :config
  (general-evil-setup t)
  (general-create-definer dl/leader-keys
    :keymaps '(normal visual emacs)
    :prefix ","))

(use-package hydra)

(use-package doom-themes
  :init (load-theme 'doom-nord t))

(setq use-dialog-box nil
    use-file-dialog nil
    cursor-type 'bar)

;; Set the default pitch face
(set-face-attribute 'default nil :font "Hack" :height 140)

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil
  :font "Hack"
  :weight 'light
  :height 140)

;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil
  :font "Helvetica"
  :height 165)

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

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("j" text-scale-increase "big")
  ("k" text-scale-decrease "small")
  ("q" nil "quit" :exit t))

(dl/leader-keys
  "t"  '(:ignore t :which-key "toggles")
  "tt" '(counsel-load-theme :which-key "choose theme")
  "ts" '(hydra-text-scale/body :which-key "scale text"))

;; Turn off UI junk
;; Note to future self: If you have problems with these later,
;; move these into custom file and set variable custom-file
(column-number-mode)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(winner-mode 1) ;; ctrl-c left, ctrl-c right for window undo/redo

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(defvar dl/black-color "#1F2528")
(defvar dl/red-color "#EC5F67")
(defvar dl/yellow-color "#FAC863")
(defvar dl/blue-color "#6699CC")
(defvar dl/green-color "#99C794")
(defvar dl/purple-color "#C594C5")
(defvar dl/teal-color "#5FB3B3")
(defvar dl/light-grey-color "#C0C5CE")
(defvar dl/dark-grey-color "#65737E")

(defun dl/org-mode-visual-fill ()
  (setq visual-fill-column-width 110
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :defer t
  :hook (org-mode . dl/org-mode-visual-fill))

(use-package ace-window
  :bind (("M-o" . ace-window))
  :custom
    (aw-scope 'frame)
    (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
    (aw-minibuffer-flag t)
  :config
    (ace-window-display-mode 1))

;; Run M-x all-the-icons-install-fonts to install
(use-package all-the-icons)
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

;; Rotates windows and layouts
(use-package rotate
  :config)

(dl/leader-keys
  "r"  '(:ignore t :which-key "rotate")
  "rw"  '(rotate-window :which-key "rotate window")
  "rl"  '(rotate-layout :which-key "rotate layout"))

(defalias 'yes-or-no-p 'y-or-n-p) ;; Use Y or N in prompts, instead of full Yes or No

(global-visual-line-mode t) ;; Wraps lines everywhere
(line-number-mode t) ;; Line numbers in the gutter
(show-paren-mode t) ;; Highlights parans for me

(defun dl/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (auto-fill-mode 0)
  (visual-line-mode 1)
  (setq evil-auto-indent nil))

(use-package org
  :defer t
  :hook (org-mode . dl/org-mode-setup)
  :config
  (setq org-edit-src-content-indentation 2 ;; Indent code blocks by 2
        org-ellipsis " ▾" ;; Prettify the fold indicator
        org-hide-emphasis-markers t ;; Hide special characters
        org-hide-block-startup nil) ;; Don't start org mode with blocks folded
  :bind
        (("C-c a" . org-agenda)))

(setq org-todo-keywords
   '((sequence "TODO(t)"
               "NEXT(n)"
               "WAITING(w)"
               "SOMEDAY(s)"
               "|"
               "CANCELED(c)"
               "DONE(d)")))

;; Fast access to tag common contexts I use
(setq org-tag-persistent-alist
  '(("Inbox" . ?i) ("@Home" . ?h) ("@Amanda" . ?a)("@Justin" . ?j)
   ("@Car" . ?c) ("@Office" . ?o) ("#Phone" . ?p) ("#Computer" . ?u)))

(setq org-todo-keyword-faces
  `(("NEXT" . ,dl/yellow-color)
   ("WAITING" . ,dl/light-grey-color)
   ("SOMEDAY" . ,dl/dark-grey-color)))

(setq org-tag-faces
  `(("@Home" . ,dl/green-color)
   ("@Car" . ,dl/purple-color)
   ("@Office" . ,dl/teal-color)
   ("Inbox" . ,dl/blue-color)

   ))

(use-package org-roam
      :straight (:host github :repo "org-roam/org-roam"
      :files (:defaults "extensions/*"))
     :init
       (setq org-roam-v2-ack t) ;; Turn off v2 warning
       (add-to-list 'display-buffer-alist
           '("\\*org-roam\\*"
             (display-buffer-in-direction)
             (direction . right)
             (window-width . 0.33)
             (window-height . fit-window-to-buffer)))
       (org-roam-db-autosync-mode)
     :custom
       (org-roam-directory (file-truename "~/Projects/Writing/Roam/"))
       (org-roam-dailies-directory "daily/")
       (org-roam-completion-everywhere t)
     :bind
       (("C-c r b" . org-roam-buffer-toggle)
        ("C-c r t" . org-roam-dailies-goto-today)
        ("C-c r y" . org-roam-dailies-goto-yesterday)
        ("C-M-n" . org-roam-node-insert)
        :map org-mode-map
        ("C-M-i"   . completion-at-point)
        ("C-M-f" . org-roam-node-find)
        ("C-M-c" . dl/org-roam-create-id)
        ("C-<left>" . org-roam-dailies-goto-previous-note)
        ("C-`" . org-roam-buffer-toggle)
        ("C-<right>" . org-roam-dailies-goto-next-note)))

(setq org-roam-dailies-capture-templates
  '(("d" "default" entry
     "* %?"
     :if-new (file+head "%<%Y-%m-%d>.org"
                        "#+TITLE: %<%Y-%m-%d>\n#+filetags: Daily\n\n"))))

(defvar dl/org-created-property-name "CREATED")

(defun dl/org-set-created-property (&optional active name)
  (interactive)
  (let* ((created (or name dl/org-created-property-name))
         (fmt (if active "<%s>" "[%s]"))
         (now (format fmt (format-time-string "%Y-%m-%d %a %H:%M"))))
    (unless (org-entry-get (point) created nil)
      (org-set-property created now)
      now)))

(defun dl/org-find-time-file-property (property &optional anywhere)
  (save-excursion
    (goto-char (point-min))
    (let ((first-heading
           (save-excursion
             (re-search-forward org-outline-regexp-bol nil t))))
      (when (re-search-forward (format "^#\\+%s:" property)
                               (if anywhere nil first-heading) t)
        (point)))))

(defun dl/org-has-time-file-property-p (property &optional anywhere)
  (when-let ((pos (dl/org-find-time-file-property property anywhere)))
    (save-excursion
      (goto-char pos)
      (if (and (looking-at-p " ")
               (progn (forward-char)
                      (org-at-timestamp-p 'lax)))
          pos -1))))

(defun dl/org-set-time-file-property (property &optional anywhere pos)
  (when-let ((pos (or pos
                      (dl/org-find-time-file-property property))))
    (save-excursion
      (goto-char pos)
      (if (looking-at-p " ")
          (forward-char)
        (insert " "))
      (delete-region (point) (line-end-position))
      (let* ((now (format-time-string "[%Y-%m-%d %a %H:%M]")))
        (insert now)))))

(defun dl/org-set-last-modified ()
  "Update the LAST_MODIFIED file property in the preamble."
  (when (derived-mode-p 'org-mode)
    (dl/org-set-time-file-property "LAST_MODIFIED")))

(defun dl/org-roam-create-id ()
  (interactive)
  (org-id-get-create)
  (dl/org-set-created-property))

(defvar current-time-format "%H:%M:%S"
  "Format of date to insert with `insert-current-time' func.
Note the weekly scope of the command's precision.")

(defun insert-current-time ()
  "insert the current time (1-week scope) into the current buffer."
       (interactive)
       (insert "* ")
       (insert (format-time-string current-time-format (current-time)))
       (insert "\n")
       )

 (dl/leader-keys
  ","  '(insert-current-time :which-key "current time"))

(setq org-roam-capture-templates
 '(("d" "default" plain
    "%?"
    :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n\n")
    :unnarrowed t)
   ("a" "area" plain
    "#+filetags: Area\n\n* Goals\n\n%^{Goals}\n\n* Tasks\n\n** TODO %?"
    :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}")
    :unnarrowed t)
   ("j" "project" plain
    "#+filetags: Project\n\n* Goals\n\n%^{Goals}\n\n* Tasks\n\n** TODO %?"
    :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}")
    :unnarrowed t)
   ("p" "people" plain
    "#+filetags: People CRM\n\n* Contacts\n\nRelationship: %^{Relationship}\nPhone:\nAddress:\nBirthday:\n\n* Notes\n\n %?"
    :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}")
    :unnarrowed t)
   ("i" "institution" plain
    "#+filetags: Institution CRM\n\n* Contacts\n\nRelationship: %^{Relationship}\nPhone:\nAddress:\n\n* Notes\n\n %?"
    :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}")
    :unnarrowed t)))

(defun dl/define-agenda-files ()
(interactive)
  "Return a list of note files containing 'HasTodo' tag. 
   I use this to denote files with tasks for org-agenda" ;
  (seq-uniq
   (seq-map
    #'car
    (org-roam-db-query
     [:select [nodes:file]
      :from tags
      :left-join nodes
      :on (= tags:node-id nodes:id)
      :where (in tag $v1)] '(["Project" "Area" "Daily"])))))

;; Roam Daily Log and Project Files only
 (setq org-agenda-files (dl/define-agenda-files))

(dl/leader-keys
  "a"  '(dl/define-agenda-files :which-key "refresh agenda db"))

(defun dl/buffer-prop-get (name)
  "Get a buffer property called NAME as a string."
  (org-with-point-at 1
    (when (re-search-forward (concat "^#\\+" name ": \\(.*\\)")
                             (point-max) t)
      (buffer-substring-no-properties
       (match-beginning 1)
       (match-end 1)))))

(defun dl/agenda-category (&optional len)
  "Get category of item at point for agenda."
  (let* ((file-name (when buffer-file-name
                      (file-name-sans-extension
                       (file-name-nondirectory buffer-file-name))))
         (title (dl/buffer-prop-get "title"))
         (category (org-get-category))
         (result
          (or (if (and
                   title
                   (string-equal category file-name))
                  title
                category)
              "")))
    (if (numberp len)
        (s-truncate len (s-pad-right len " " result))
      result)))

(setq org-agenda-prefix-format
      '((agenda . " %i %(dl/agenda-category 32)%?-32t% s")
        (todo . " %i %(dl/agenda-category 32) ")
        (tags . " %i %(dl/agenda-category 32) ")
        (search . " %i %(dl/agenda-category 32) ")))

(use-package org-super-agenda
   :after org-agenda
   :init
 (setq org-agenda-dim-blocked-tasks nil))

 ;; Dashboard View
 (setq org-super-agenda-groups
      '((:name "Priority"
               :priority "A")
        (:name "Inbox"
               :tag ("Inbox" "Daily"))
        (:name "Next Actions at Office"
               :and (
               :todo ("NEXT")
               :tag ("Active")
               :tag ("@Office")))
        (:name "Next Actions at Home"
               :and (
               :todo ("NEXT")
               :tag ("Active")
               :tag ("@Home")))
        (:name "Waiting"
               :todo "WAITING")
        (:name "Maintenance"
               :todo "MAINTAIN")
        (:name "Home"
               :tag "@Home")
        (:name "Office"
               :tag "@Office")
        (:name "Braeview"
               :tag "Braeview")
        (:name "Productivity"
               :tag "Productivity")
        (:name "Someday"
               :todo "SOMEDAY")))

 (org-super-agenda-mode)

(use-package org-superstar
  :after org
  :hook (org-mode . org-superstar-mode)
  :custom
    (org-superstar-remove-leading-stars t)
    (org-superstar-headline-bullets-list '("•" "•" "•" "◦" "◦" "◦" "◦")))

;; Not sure why this is needed, but the org-indent face "requires" it (pun)
(require 'org-indent)

(set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
(set-face-attribute 'org-table nil  :inherit 'fixed-pitch)
(set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
(set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))
(set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
(set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
(set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)

(set-face-attribute 'org-document-title nil :font "SF Pro Display" :weight 'bold :height 1.2)
(dolist (face '((org-level-1 . 1.2)
                (org-level-2 . 1.15)
                (org-level-3 . 1.1)
                (org-level-4 . 1.05)
                (org-level-5 . 1.05)
                (org-level-6 . 1.0)
                (org-level-7 . 1.0)
                (org-level-8 . 1.0)))
  (set-face-attribute (car face) nil :font "SF Pro Display" :weight 'medium :height (cdr face)))

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
  :hook
    (org-mode . (lambda () evil-org-mode))
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

(let ((code_dir_path '"\"~/Projects/Code\""))
(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom
      ((projectile-completion-system 'ivy))
    :bind-keymap
      ("C-c p" . projectile-command-map)
    :init
      (when (file-directory-p code_dir_path)
      (setq projectile-project-search-path '(code_dir_path)))
	(setq projectile-switch-project-action #'projectile-dired))

;; Gives me Ivy options in the Projectile menus
(use-package counsel-projectile
  :after projectile
  :config (counsel-projectile-mode))
)

(setq backup-directory-alist
  `(("." . ,(concat user-emacs-directory "backup")))
    backup-by-copying t    ; Don't delink hardlinks
    version-control t      ; Use version numbers on backups
    delete-old-versions t  ; Automatically delete excess backups
    kept-new-versions 20   ; how many of the newest versions to keep
    kept-old-versions 5    ; and how many of the old
  )

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

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

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

;; Auto complete HTML and CSS tags
(use-package emmet-mode)
(add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.

;; Minor mode to show HTML hex colors
(use-package rainbow-mode)

(use-package lsp-mode
  :commands lsp lsp-deferred
  :init
  (setq lsp-keymap-prefix "C-c l")
  (setq lsp-enable-which-key-integration t))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
    (lsp-ui-doc-position 'bottom))

;;(use-package keytar) ;; M-x install-keytar
;;(use-package lsp-grammarly
;;  :ensure t
;;  :hook (text-mode . (lambda ()
;;                       (require 'lsp-grammarly)
;;                       (lsp-deferred))))

(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
        ("<tab>" . company-complete-selection))
        (:map lsp-mode-map
        ("<tab>" . company-indent-or-complete-common))
   :custom
     (company-minimum-prefix-length 1)
     (company-idle-delay 0.0))

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2))

;;(add-to-list 'lsp-language-id-configuration '(org-mode . "plaintext"))

(use-package magit
  :commands (magit-status magit-get-current-branch))

;; This uses Github Flavored Markdown for README files
(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
    ("\\.md\\'" . markdown-mode)
    ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(setq org-src-tab-acts-natively nil)

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
