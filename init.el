;; Straight
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
(setq org-agenda-files '("~/Dropbox/org"))
(setq org-tag-alist '(("@writing" . ?w) ("@research" . ?r) ("personal" . ?p)))
(setq org-odt-preferred-output-format "docx")
;; Org-Journal
(straight-use-package 'org-journal)
(setq org-journal-dir "~/Dropbox/org/journal/")
(setq org-journal-date-format "%A, %d %B %Y")
(require 'org-journal)
(add-hook 'org-journal-hook 'turn-on-flyspell)

;; Org-ref
(straight-use-package 'org-ref)
(straight-use-package 'helm-bibtex)
(require 'org-ref)
(setq reftex-default-bibliography '("~/Dropbox/bibliography/references.bib"))

;; see org-ref for use of these variables
(setq org-ref-bibliography-notes "~/Dropbox/bibliography/notes.org"
      org-ref-default-bibliography '("~/Dropbox/bibliography/references.bib")
      org-ref-pdf-directory "~/Dropbox/bibliography/bibtex-pdfs/")
(setq bibtex-completion-bibliography "~/Dropbox/bibliography/references.bib"
      bibtex-completion-library-path "~/Dropbox/bibliography/bibtex-pdfs"
      bibtex-completion-notes-path "~/Dropbox/bibliography/helm-bibtex-notes")

;; open pdf with system pdf viewer (works on mac)
(setq bibtex-completion-pdf-open-function
  (lambda (fpath)
    (start-process "open" "*open*" "open" fpath)))

;; alternative
;; (setq bibtex-completion-pdf-open-function 'org-open-file)

;; Org Roam
(straight-use-package 'org-roam)
(executable-find "sqlite3")

;; Writerroom Mode
(straight-use-package 'writeroom-mode)

;; AUCTeX
(straight-use-package 'auctex)

(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)

;; RefTeX
(require 'reftex)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)   ; with AUCTeX LaTeX mode
(add-hook 'Latex-mode-hook 'turn-on-reftex)   ; with Emacs latex mode

(add-hook 'LaTeX-mode-hook 'turn-on-flyspell) ; Flyspell

;; Markdown
(straight-use-package 'markdown-mode)


;; Helm
(straight-use-package 'helm)
(global-set-key (kbd "M-x") 'helm-M-x)

;; Elpy
(straight-use-package 'elpy)
(setenv "WORKON_HOME" "~/miniforge3/envs/")
(elpy-enable)
(straight-use-package 'jedi)

;; STAN
(straight-use-package 'stan-mode)
(straight-use-package 'company-stan)
(straight-use-package 'eldoc-stan)
(straight-use-package 'flycheck-stan)
(straight-use-package 'stan-snippets)

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

;; ESS
(straight-use-package 'ess)

;; LSP
(straight-use-package 'lsp-mode)
(straight-use-package 'lsp-ui)
(straight-use-package 'company)
(straight-use-package 'lsp-treemacs)
(straight-use-package 'helm-lsp)
(straight-use-package 'dap-mode)
(straight-use-package 'which-key)

(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (R-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

;; optionally
(use-package lsp-ui :commands lsp-ui-mode)
;; if you are helm user
(use-package helm-lsp :commands helm-lsp-workspace-symbol)
;; if you are ivy user
;; (use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
;; (use-package lsp-treemacs :commands lsp-treemacs-errors-list)

;; optionally if you want to use debugger
(use-package dap-mode)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language

;; optional if you want which-key integration
(use-package which-key
    :config
    (which-key-mode))

;; try mu4e
(require 'mu4e)
;; use mu4e for e-mail in emacs
(setq mail-user-agent 'mu4e-user-agent)

;; these are actually the defaults
(setq
  mu4e-sent-folder   "/Inviata"       ;; folder for sent messages
  mu4e-drafts-folder "/Bozze"     ;; unfinished messages
  mu4e-trash-folder  "/Cestino"      ;; trashed messages
  mu4e-refile-folder "/archive")   ;; saved messages

(setq mu4e-get-mail-command "mbsync -a")
;; tell message-mode how to send mail

(setq mu4e-compose-reply-to-address "mascaretti@stat.unipd.it"
      user-email-address "mascaretti@stat.unipd.it"
      user-full-name "Andrea Mascaretti")

(setq mu4e-compose-signature
      "Andrea Mascaretti")

;; SMTP MAIL SETTING
(setq
   message-send-mail-function 'smtpmail-send-it
   smtpmail-default-smtp-server "smtp.stat.unipd.it"
   smtpmail-smtp-server "smtp.stat.unipd.it"
   smtpmail-smtp-service 465
   smtpmail-local-domain "unipd.it")

   ;; if you need offline mode, set these -- and create the queue dir
   ;; with 'mu mkdir', i.e.. mu mkdir /home/user/Maildir/queue
   ;; smtpmail-queue-mail  nil
   ;; smtpmail-queue-dir  "/home/user/Maildir/queue/cur")

;; don't keep message buffers around
(setq message-kill-buffer-on-exit t)

