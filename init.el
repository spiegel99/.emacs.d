(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;;basic configuration
(setq inhibit-startup-message t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(global-display-line-numbers-mode 1)
(global-hl-line-mode 1)
;;replace yes or no by 'y' or 'n'
(fset 'yes-or-no-p 'y-or-n-p)
(setq visible-bell t)

(set-frame-font "Iosevka Term 17" nil t)
(load-theme 'modus-vivendi)

;send auto-save files to another directory
(setq backup-directory-alist '(("." . "~/backup")))
(with-eval-after-load 'tramp
      (add-to-list 'tramp-backup-directory-alist
		   (cons tramp-file-name-regexp nil)))

(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(load-theme 'jbeans t)

;;ui improvements
(use-package swiper
  :ensure t)
  
(use-package ivy
  :ensure t
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
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (setq ivy-use-selectable-prompt t)
  (ivy-mode 1))

(use-package counsel
  :bind (("C-x b" . 'counsel-switch-buffer)
         ("C-x f" . 'counsel-find-file)
	 ("M-x" . 'counsel-M-x)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :config
  (counsel-mode 1))

(use-package ivy-rich
  :ensure t
  :init
  (ivy-rich-mode 1))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package which-key
  :defer 0
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 1))

;; install fonts, run all-the-icons-install-fonts just after, see doc
(use-package all-the-icons
  :if (display-graphic-p)
  :ensure t)

(use-package all-the-icons
  :if (display-graphic-p)
  :ensure t)

(use-package all-the-icons-dired
  :ensure t
  :hook ((dired-mode . all-the-icons-dired-mode)
         (dired-mode . dired-hide-details-mode)))
