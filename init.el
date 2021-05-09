; Straight
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

;; Magit
(straight-use-package 'magit)

;; Use-package
(straight-use-package 'use-package)

;; Org

(add-hook 'org-mode-hook 'turn-on-flyspell)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(setq org-log-done 'note)
(setq org-directory "~/Dropbox/org")
(setq org-agenda-files '("~/Dropbox/org"))
(setq org-tag-alist '(("@writing" . ?w) ("@research" . ?r) ("personal" . ?p)))
(setq org-odt-preferred-output-format "docx")

(setq org-capture-templates '(("t" "Todo [inbox]" entry
                               (file+headline "~/Dropbox/org/inbox.org" "Tasks")
                               "* TODO %i%?")
                              ("T" "Tickler" entry
                               (file+headline "~/Dropbox/org/tickler.org" "Tickler")
                               "* %i%? \n %U")))

(setq org-refile-targets '(("~/Dropbox/org/gtd.org" :maxlevel . 3)
                           ("~/Dropbox/org/someday.org" :level . 1)
                           ("~/Dropbox/org/tickler.org" :maxlevel . 2)))


(setq org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))

;; Org-Journal
(straight-use-package 'org-journal)
(setq org-journal-dir "~/Dropbox/org/journal/")
(setq org-journal-date-format "%A, %d %B %Y")
(require 'org-journal)
(add-hook 'org-journal-hook 'turn-on-flyspell)

;; Org-ref
(straight-use-package 'org-ref)
(require 'org-ref)
(setq reftex-default-bibliography '("~/gdrive/phd/references/zotLib.bib"))

;; see org-ref for use of these variables
(setq org-ref-bibliography-notes "~/gdrive/phd/references/references/notes.org"
      org-ref-default-bibliography '("~/gdrive/phd/references/zotLib.bib")
      org-ref-pdf-directory "~/gdrive/phd/zotero_files")
(setq bibtex-completion-bibliography "~/gdrive/phd/references/zotLib.bib"
      bibtex-completion-library-path "~/gdrive/phd/zotero_files/"
      bibtex-completion-pdf-field "File"
      bibtex-completion-notes-path "~/gdrive/phd/references/helm-bibtex-notes")



(defun my/org-ref-open-pdf-at-point ()
  "Open the pdf for bibtex key under point if it exists."
  (interactive)
  (let* ((results (org-ref-get-bibtex-key-and-file))
         (key (car results))
	 (pdf-file (car (bibtex-completion-find-pdf key))))
    (if (file-exists-p pdf-file)
	(org-open-file pdf-file)
      (message "No PDF found for %s" key))))

(setq org-ref-open-pdf-function 'my/org-ref-open-pdf-at-point)

;; open pdf with system pdf viewer (works on mac)
;; (setq bibtex-completion-pdf-open-function
;;   (lambda (fpath)
;;    (start-process "open" "*open*" "open" fpath)))

;; alternative
(setq bibtex-completion-pdf-open-function 'org-open-file)

;; Org-babel
(org-babel-do-load-languages
 'org-babel-load-languages
 '((R . t)))

;; Helm-Bibtex
(straight-use-package 'helm-bibtex)
(setq bibtex-completion-notes-path "~/gdrive/phd/pdf_notes/notes.org")

;; Org-Roam
(straight-use-package 'org-roam)
(setq org-roam-directory "~/Dropbox/org-roam")
(add-hook 'after-init-hook 'org-roam-mode)
(setq org-roam-index-file "~/Dropbox/org-roam.index.org")

(straight-use-package 'org-roam-bibtex)
(use-package org-roam-bibtex
  :after org-roam
  :hook (org-roam-mode . org-roam-bibtex-mode)
  :config
  (require 'org-ref)) ; optional: if Org Ref is not loaded anywhere else, load it here

(setq orb-templates
      '(("r" "ref" plain (function org-roam-capture--get-point) ""
         :file-name "${citekey}"
         :head "#+TITLE: ${citekey}: ${title}\n#+ROAM_KEY: ${ref}\n" ; <--
         :unnarrowed t)))
(setq org-roam-capture-templates
      '(("d" "default" plain (function org-roam--capture-get-point)
     "%?"
     :file-name "%<%Y%m%d%H%M%S>-${slug}"
     :head "#+title: ${title}\n"
     :unnarrowed t)))

(setq orb-note-actions-interface 'hydra)


;; Deft
(straight-use-package 'deft)
(use-package deft
  :after org
  :bind
  ("C-c n d" . deft)
  :custom
  (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-default-extension "org")
  (deft-directory "~/Dropbox/org-roam/"))


;; Org-downaload
(straight-use-package 'org-download)
(use-package org-download
  :after org
  :bind
  (:map org-mode-map
        (("s-Y" . org-download-screenshot)
         ("s-y" . org-download-yank))))

;; Mathpix
(use-package mathpix.el
  :straight (:host github :repo "jethrokuan/mathpix.el")
  :custom ((mathpix-app-id "app-id")
           (mathpix-app-key "app-key"))
  :bind
  ("C-x m" . mathpix-screenshot))

;; AUCTeX
(straight-use-package 'auctex)

(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)

;; Use pdf-tools to open PDF files
(setq TeX-view-program-selection '((output-pdf "PDF Tools"))
      TeX-source-correlate-start-server t)

;; Update PDF buffers after successful LaTeX runs
(add-hook 'TeX-after-compilation-finished-functions
           #'TeX-revert-document-buffer)

(straight-use-package 'cdlatex)
(add-hook 'LaTeX-mode-hook 'turn-on-cdlatex) ; with Auctex


;; RefTeX
(require 'reftex)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)   ; with AUCTeX LaTeX mode
(add-hook 'Latex-mode-hook 'turn-on-reftex)   ; with Emacs latex mode
(setq reftex-plug-into-AUCTeX t)

(add-hook 'LaTeX-mode-hook 'turn-on-flyspell) ; Flyspell

;; PDF TOOLS
(straight-use-package 'pdf-tools)
(add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
(straight-use-package 'org-noter)
(pdf-loader-install)


;; ESS
(straight-use-package 'ess)


;; PROJECTILE
(straight-use-package 'projectile)
(require 'projectile)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(projectile-mode +1)

;; HELM
(straight-use-package 'helm)
(global-set-key (kbd "M-x") #'helm-M-x)
(global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
(global-set-key (kbd "C-x C-f") #'helm-find-files)
(helm-mode 1)

;; Company
(straight-use-package 'company)
(add-hook 'after-init-hook 'global-company-mode)


;; YASnippet
(straight-use-package 'yasnippet)
(straight-use-package 'yasnippet-snippets)
(yas-global-mode 1)

;; Stan

(straight-use-package 'stan-mode)
(straight-use-package 'company-stan)
(straight-use-package 'eldoc-stan)
(straight-use-package 'flycheck-stan)
(straight-use-package 'stan-snippets)

;; Uncomment the line below if not required elsewhere.
;; (require 'use-package)

;;; stan-mode.el
(use-package stan-mode
  :mode ("\\.stan\\'" . stan-mode)
  :hook (stan-mode . stan-mode-setup)
  ;;
  :config
  ;; The officially recommended offset is 2.
  (setq stan-indentation-offset 2))

;;; company-stan.el
(use-package company-stan
  :hook (stan-mode . company-stan-setup)
  ;;
  :config
  ;; Whether to use fuzzy matching in `company-stan'
  (setq company-stan-fuzzy nil))

;;; eldoc-stan.el
(use-package eldoc-stan
  :hook (stan-mode . eldoc-stan-setup)
  ;;
  :config
  ;; No configuration options as of now.
  )

;;; flycheck-stan.el
(use-package flycheck-stan
  ;; Add a hook to setup `flycheck-stan' upon `stan-mode' entry
  :hook ((stan-mode . flycheck-stan-stanc2-setup)
         (stan-mode . flycheck-stan-stanc3-setup))
  :config
  ;; A string containing the name or the path of the stanc2 executable
  ;; If nil, defaults to `stanc2'
  (setq flycheck-stanc-executable nil)
  ;; A string containing the name or the path of the stanc2 executable
  ;; If nil, defaults to `stanc3'
  (setq flycheck-stanc3-executable nil))

;;; stan-snippets.el
(use-package stan-snippets
  :hook (stan-mode . stan-snippets-initialize)
  ;;
  :config
  ;; No configuration options as of now.
  )

;;; ac-stan.el (Not on MELPA; Need manual installation)
(use-package ac-stan
  :load-path "path-to-your-directory/ac-stan/"
  ;; Delete the line below if using.
  :disabled t
  :hook (stan-mode . stan-ac-mode-setup)
  ;;
  :config
  ;; No configuration options as of now.
  )

;; PYTHON
(straight-use-package 'anaconda-mode)
(straight-use-package 'elpy)
(use-package elpy
  :ensure t
  :init
  (elpy-enable))

