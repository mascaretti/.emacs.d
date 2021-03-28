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
(straight-use-package 'ivy-bibtex)
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
(setq bibtex-completion-pdf-open-function
  (lambda (fpath)
    (start-process "open" "*open*" "open" fpath)))

;; alternative
;; (setq bibtex-completion-pdf-open-function 'org-open-file)


;; Org-Roam
(straight-use-package 'org-roam)
(setq org-roam-directory "~/Dropbox/org-roam")
(add-hook 'after-init-hook 'org-roam-mode)

(straight-use-package 'org-roam-bibtex)
(use-package org-roam-bibtex
  :after org-roam
  :hook (org-roam-mode . org-roam-bibtex-mode)
  :config
  (require 'org-ref)) ; optional: if Org Ref is not loaded anywhere else, load it here

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

;; PROJECTILE
(straight-use-package 'projectile)
(require 'projectile)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(projectile-mode +1)

;; IVY
(straight-use-package 'counsel)
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "(%d/%d) ")

(global-set-key (kbd "C-s") 'swiper-isearch)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "M-y") 'counsel-yank-pop)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(global-set-key (kbd "<f2> j") 'counsel-set-variable)
(global-set-key (kbd "C-x b") 'ivy-switch-buffer)
(global-set-key (kbd "C-c v") 'ivy-push-view)
(global-set-key (kbd "C-c V") 'ivy-pop-view)

(global-set-key (kbd "C-c c") 'counsel-compile)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c L") 'counsel-git-log)
(global-set-key (kbd "C-c k") 'counsel-rg)
(global-set-key (kbd "C-c m") 'counsel-linux-app)
(global-set-key (kbd "C-c n") 'counsel-fzf)
(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "C-c J") 'counsel-file-jump)
(global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
(global-set-key (kbd "C-c w") 'counsel-wmctrl)

(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "C-c b") 'counsel-bookmark)
(global-set-key (kbd "C-c d") 'counsel-descbinds)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c o") 'counsel-outline)
(global-set-key (kbd "C-c t") 'counsel-load-theme)
(global-set-key (kbd "C-c F") 'counsel-org-file)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files
   '("~/Dropbox/org/projects.org" "/home/andrea/Dropbox/org/Dreams.org" "/home/andrea/Dropbox/org/README.org" "/home/andrea/Dropbox/org/blog.org" "/home/andrea/Dropbox/org/francais.org" "/home/andrea/Dropbox/org/gtd.org" "/home/andrea/Dropbox/org/inbox.org" "/home/andrea/Dropbox/org/learning.org" "/home/andrea/Dropbox/org/someday.org" "/home/andrea/Dropbox/org/tickler.org" "/home/andrea/Dropbox/org/writing.org")))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
