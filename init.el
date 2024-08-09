;;Options
(setq inhibit-startup-screen t)
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
  :init (doom-modeline-mode 1))
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

(general-create-definer voxalium/leader-keys
  :keymaps '(normal insert visual emacs)
  :prefix "SPC"
  :global-prefix "C-SPC")
(voxalium/leader-keys
 "e" '(dired :which-key "Open dired")
 "x" '(delete-window :which-key "Delete window")
 "b" '(counsel-switch-buffer :which-key "Switch buffer")
 "s h" '(split-window-horizontally :which-key "Split horizontally")
 "s v" '(split-window-vertically :which-key "Split vertically")
 ))

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




(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(counsel-projectile projectile hydra evil-collection general which-key rainbow-delimiters magit lsp-mode ivy-rich helpful evil doom-themes doom-modeline counsel all-the-icons)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
