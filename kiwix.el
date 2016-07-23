;;; kiwix.el --- Kiwix interface and support for Emacs.
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:


(defgroup kiwix nil
  "kiwix group."
  :group 'custom-group)

(defcustom kiwix-browser "google-chrome-stable"
  "Specify browser for Kiwix visiting."
  :group 'kiwix)

(defcustom kiwix-default-library "wikipedia_en_all_2016-02"
  "Specify the default Kiwix library you want to search."
  :group 'kiwix)

;; launch Kiwix server
;;;###autoload
(defun kiwix-launch-server ()
  "Launch Kiwix server."
  (interactive)
  
  (let ((kiwix-server-command "/usr/lib/kiwix/bin/kiwix-serve ")
        (kiwix-server-library-option "--library ")
        (kiwix-server-port "--port=8000 ")
        (kiwix-server-daemon "--daemon ")
        (kiwix-server-library-path (concat (getenv "HOME") "/.www.kiwix.org/kiwix/8ip89lik.default/data/library/library.xml"))
        )
    (async-shell-command
     (concat kiwix-server-command kiwix-server-library-option kiwix-server-port kiwix-server-daemon kiwix-server-library-path))))


(defun kiwix-query (query)
  "Search `QUERY' with Kiwix."
  (let* ((browser kiwix-browser)
         (kiwix-server "http://127.0.0.1:8000/")
         (kiwix-library kiwix-default-library)
         (url (concat kiwix-server kiwix-library "/A/" (capitalize query) ".html")))
    (shell-command (concat browser " " url))))

;;;###autoload
(defun kiwix-at-point (&optional input)
  "Search for the symbol at point with `kiwix-query' with `INPUT'."
  (interactive "P")
  (let* ((query-string (if mark-active
                           (buffer-substring (region-beginning) (region-end))
                         (thing-at-point 'symbol))))
    (kiwix-query (if (or input (null query-string))
                     (read-string "Kiwix search: " query-string-input)
                   query-string))))

;;; Support Org-mode
;; [[wiki:]]
;; for open wiki search query with local application database.
(defalias 'org-wiki-link-open 'kiwix-search)

(org-add-link-type "wiki" 'org-wiki-link-open)

;; [[Wikipedia_Local:]]
(add-to-list
 org-link-abbrev-alist
 '("Wikipedia_Local" . "http://127.0.0.1:8000/wikipedia_zh_all_2015-11/A/%s.html"))


(provide 'kiwix)

;;; kiwix.el ends here
