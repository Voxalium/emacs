;;Options
(setq inhibit-startup-screen t)
(setq backup-directory-alist '(("." . "~/.emacs.d/saves")))
(tool-bar-mode 0)
(tooltip-mode 0)
(scroll-bar-mode 0)
(menu-bar-mode 0)
(set-fringe-mode 0)
(recentf-mode 1)
(save-place-mode 1)
(setq use-dialob-box nil)
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffer t)
(column-number-mode 1)
(global-display-line-numbers-mode 1)
(setq display-line-numbers 'relative)
(dolist (mode '(org-mode-hook
		term-mode-hook
		eshell-mode-hook
		shell-mode-hook
		))
  (add-hook mode (lambda() (display-line-numbers-mode 0))))

;;Theme
(set-face-attribute 'default nil :font "FiraCode Nerd Font Mono" :height 120)

;;Packages
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)

(unless package-archive-contents (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history))
  :config (setq ivy-initial-inputs-alist nil))
(use-package ivy
  :init
  (ivy-mode 1)
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)	
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill)))
(use-package ivy-rich
  :init (ivy-rich-mode 1))
(use-package which-key
  :init (which-key-mode)
  :diminish
  :config(setq which-key-idle-delay 0.3))

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  (setq doom-modeline-height 15)
  (setq doom-modeline-major-mode-icon nil)
  (setq doom-modeline-modal-modern-icon nil)
  (setq doom-modeline-buffer-encoding nil)
  )
(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))
(use-package lsp-mode)
(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-function))
(use-package all-the-icons)
(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
	doom-themes-enable-italic t)
  (load-theme 'doom-tokyo-night t)
  ) 

;;Keymaps
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(windmove-default-keybindings)
(use-package general
:config (general-evil-setup t)

(general-create-definer keymaps/leader-keys
  :keymaps '(normal insert visual emacs)
  :prefix "SPC"
  :global-prefix "C-SPC")
(keymaps/leader-keys
 "e" '(dired :which-key "Open dired")
 "x" '(delete-window :which-key "Delete window")
 "b" '(counsel-switch-buffer :which-key "Switch buffer")
 "s h" '(split-window-horizontally :which-key "Split horizontally")
 "s v" '(split-window-vertically :which-key "Split vertically")
 "f f" '(counsel-find-file :which-key "Find files")
 "f p" '(projectile-switch-project :which-key "Switch projects")
 ))

(use-package drag-stuff
  :init
  (drag-stuff-global-mode 1)
  (drag-stuff-define-keys)
  )

(use-package evil-collection
  :after evil
  :config (evil-collection-init))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1)
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
  )

(use-package undo-tree)
(global-undo-tree-mode)
(setq evil-undo-system 'undo-tree)

(setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))
(use-package hydra)
(defhydra hydra-zoom (global-map "<f2>" )
	  "zoom"
	  ("j" text-scale-increase "in")
	  ("k" text-scale-decrease "out"))

	  
(use-package projectile
  :init
  (projectile-mode +1)
  (when (file-directory-p "~/Documents/Dev/Code")
    (setq projectile-project-search-path '("~/Documents/Dev/Code")))
  (setq projectile-switch-project-action #'projectile-dired)
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map)))
(use-package counsel-projectile
  :config (counsel-projectile-mode))

(use-package forge)
(use-package ghub)

(defun voxalium/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))
(defun voxalium/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "FiraCode Nerd Font Mono" :weight 'regular :height (cdr face)))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))
(use-package org
  :hook (org-mode . voxalium/org-mode-setup)
  :config
  (setq org-ellipsis " +"
	org-hide-emphasis-markers t)
	(voxalium/org-font-setup)
	)
  
(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(defun voxalium/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . voxalium/org-mode-visual-fill))


(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
