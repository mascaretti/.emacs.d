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

;; Hugo + Org mode
(straight-use-package 'ox-hugo)
(use-package ox-hugo
  :after ox)
(setq HUGO-BASE-DIR "git/github/phd-blog")

;; Org Capture
;; Populates only the EXPORT_FILE_NAME property in the inserted headline.
(with-eval-after-load 'org-capture
  (defun org-hugo-new-subtree-post-capture-template ()
    "Returns `org-capture' template string for new Hugo post.
See `org-capture-templates' for more information."
    (let* ((title (read-from-minibuffer "Post Title: ")) ;Prompt to enter the post title
           (fname (org-hugo-slug title)))
      (mapconcat #'identity
                 `(
                   ,(concat "* TODO " title)
                   ":PROPERTIES:"
                   ,(concat ":EXPORT_FILE_NAME: " fname)
                   ":END:"
                   "%?\n")          ;Place the cursor here finally
                 "\n")))

  (add-to-list 'org-capture-templates
               '("h"                ;`org-capture' binding + h
                 "Hugo post"
                 entry
                 ;; It is assumed that below file is present in `org-directory'
                 ;; and that it has a "Blog Ideas" heading. It can even be a
                 ;; symlink pointing to the actual location of blog.org!
                 (file+olp "blog.org" "Blog Ideas")
                 (function org-hugo-new-subtree-post-capture-template))))


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
(setq reftex-default-bibliography '("~/gdrive/phd/references/zotLib.bib"))

;; see org-ref for use of these variables
(setq org-ref-bibliography-notes "~/gdrive/phd/references/references/notes.org"
      org-ref-default-bibliography '("~/gdrive/phd/references/zotLib.bib")
      org-ref-pdf-directory "~/gdrive/phd/references/bibtex-pdfs/")
(setq bibtex-completion-bibliography "~/gdrive/phd/references/zotLib.bib"
      bibtex-completion-library-path "~/gdrive/phd/references/bibtex-pdfs"
      bibtex-completion-notes-path "~/gdrive/phd/references/helm-bibtex-notes")

;; open pdf with system pdf viewer (works on mac)
(setq bibtex-completion-pdf-open-function
  (lambda (fpath)
    (start-process "open" "*open*" "open" fpath)))

;; alternative
;; (setq bibtex-completion-pdf-open-function 'org-open-file)


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
