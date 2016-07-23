;;; kiwix-http.el
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:


(require 'cl-lib)

;; FIXME:
;; should use a standard where function to find path.
;; you may can define a function instead or a constant holding the relative path, then make use of it when needed.
(defcustom kiwix-search-command "/usr/lib/kiwix/bin/kiwix-search"
  "The kiwix-search command path."
  :type 'string
  :group 'kiwix
  :safe 'stringp)
(defcustom kiwix-data-profile-path "~/.www.kiwix.org/kiwix/8ip89lik.default/"
  "The kiwix profile data directory."
  :type 'string
  :group 'kiwix
  :safe 'stringp)

;; TODO: replace `browse-url'
(defcustom kiwix-browser-function 'browse-url-conkeror
  "Specify browser function to open kiwix search result."
  :type 'function
  :group 'kiwix
  :safe 'functionp)

(defvar kiwix-data-index-path nil)

(setq kiwix-data-index-path (concat kiwix-data-profile-path "data/index/"))

(defun kiwix-serve-index ()
  "Open kiwix HTTP server index page."
  (interactive)
  (browse-url "http://127.0.0.1:8000")
  )

(defun kiwix-libraries ()
  "Get a list of dirs under a specific dir."
  ;; ~/.www.kiwix.org/kiwix/8ip89lik.default/data/content/*.zim
  (let ((libraries
         ;; TODO: use a filter function from cl-lib or dash to replace `mapcar'.
         (mapcar #'(lambda (file)
                     (let ((zim-file (string-match-p ".*\\.zim\\'" file)))
                       zim-file))
                 (directory-files kiwix-data-index-path)
                 )))
    libraries))

(defun kiwix-serve-search (library query)
  "Execute shell command `kiwix-search' on `LIBRARY' with `QUERY'.
Return a list of results."

  ;; TODO: don't use shell-command.
  ;; use execute a process sync/async. check out in Elisp info.
  (let ((search-results (shell-command
                         (concat kiwix-search-command " "
                                 kiwix-data-profile-path kiwix-data-index-path
                                 library " " query
                                 " 2> /dev/null"))))
    search-results)
  )

(defun kiwix-serve-query (library)
  "Query kiwix HTTP server in a specific `LIBRARY'."
  (interactive ; FIXME: Don't know where is wrong yet.
   (list (completing-read
          "Kiwix Library: "
          (let (libraries)
            (dolist (elt (kiwix-libraries))
              (setq libraries (append libraries (list (car elt)))))
            libraries))))
  
  (let ((query (read-from-minibuffer "Kiwix Query: "))
        (results (kiwix-search library query))
        (result (interactive
                 (list (completing-read
                        "select a result to visit: "
                        results)))))
    (browse-url
     (concat "http://127.0.0.1:8000/" library "/A/" result))
    )
  )

;; TODO: finnally, write a function to reterive the values from the last command.

;; final function
;;;###autoload
(defun kiwix-at-point (&optional edit-search)
  "Search for the word at point in Kiwix."
  (interactive "P")
  (let* ((thing (if mark-active
                    (buffer-substring (region-beginning) (region-end))
                  (thing-at-point 'symbol)))
         (search (kiwix-maybe-specify-library thing)))
    (kiwix-run-search
     (if (or edit-search (null thing))
         (read-string "Kiwix search: " search)
       search))))

;; TODO: add org-mode protocol support: `wiki_offline:'


(provide 'kiwix-http)

;;; kiwix-http.el ends here
