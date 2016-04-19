;;; kiwix-http.el
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:


(defcustom kiwix-search-command "/usr/lib/kiwix/bin/kiwix-search"
  "The kiwix-search command path."
  :group 'kiwix)
(defcustom kiwix-data-profile-path "~/.www.kiwix.org/kiwix/8ip89lik.default/"
  "The kiwix profile data directory."
  :group 'kiwix)

(defvar kiwix-data-index-path nil)

(setq kiwix-data-index-path (concat kiwix-data-profile-path "data/index/"))

(defun kiwix-serve-index ()
  "Open kiwix HTTP server index page."
  (interactive)
  ;; (browse-url "http://127.0.0.1:8000")
  (browse-url-conkeror "http://127.0.0.1:8000")
  )

(defun kiwix-libraries ()
  "Get a list of dirs under a specific dir."
  ;; ~/.www.kiwix.org/kiwix/8ip89lik.default/data/content/*.zim
  (let ((libraries
         ;; TODO: filter out ZIM files as libraries.
         (mapcar #'(lambda (file)
                     (let ((zim-file (string-match-p ".*\\.zim" file)))
                       zim-file))
                 (directory-files kiwix-data-index-path)
                 )))
    libraries))

(defun kiwix-serve-search (library query)
  "Execute shell command `kiwix-search' on `LIBRARY' with `QUERY'.
Return a list of results."
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
    (browse-url-conkeror
     (concat "http://127.0.0.1:8000/" library "/A/" result))
    )
  )



(provide 'kiwix-http)

;;; kiwix-http.el ends here
