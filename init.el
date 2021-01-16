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


;; Org

(add-hook 'org-mode-hook 'turn-on-flyspell)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(setq org-log-done 'note)
(setq org-agenda-files '("~/Dropbox/org"))
(setq org-tag-alist '(("@writing" . ?w) ("@research" . ?r) ("personal" . ?p)))

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
