;;; kiwix.el --- Kiwix interface and support for Emacs.
;;; -*- coding: utf-8 -*-

;; Author: stardiviner <numbchild@gmail.com>
;; Maintainer: stardiviner <numbchild@gmail.com>
;; Keywords: kiwix wikipedia
;; URL: https://github.com/stardiviner/kiwix.el
;; Created: 23th July 2016
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.3") (cl-lib "2"))

;;; Commentary:

;;; This currently only works for Linux, not tested for Mac OS X and Windows.

;;; Usage:
;;
;; [M-x kiwix-launch-server] to launch Kiwix server.
;; [M-x kiwix-at-point] to search the word under point or the region selected string.

;;; Code:


(defgroup kiwix nil
  "Kiwix customization options.")

(defcustom kiwix-server-command "/usr/lib/kiwix/bin/kiwix-serve "
  "Specify kiwix server command."
  :type 'string
  :group 'kiwix)

(defcustom kiwix-default-data-path "8ip89lik.default"
  "Specify the default Kiwix data profile path."
  :type 'string
  :group 'kiwix)

(defcustom kiwix-default-library "wikipedia_en_all_2016-02"
  "Specify the default Kiwix library you want to search."
  :type 'string
  :group 'kiwix)

(defcustom kiwix-server-port "8000"
  "Specify the default Kiwix server port."
  :type 'string
  :group 'kiwix)

;; launch Kiwix server
;;;###autoload
(defun kiwix-launch-server ()
  "Launch Kiwix server."
  (interactive)
  
  (let ((library "--library ")
        (port (concat "--port=" kiwix-server-port " "))
        (daemon "--daemon ")
        (library-path (shell-quote-argument (concat (getenv "HOME") "/.www.kiwix.org/kiwix/" kiwix-default-data-path "/data/library/library.xml")))
        )
    (async-shell-command
     (concat kiwix-server-command library port daemon library-path))))


(defun kiwix-query (query)
  "Search `QUERY' with Kiwix."
  (let* ((kiwix-server "http://127.0.0.1:8000/")
         (kiwix-library kiwix-default-library)
         (url (concat kiwix-server kiwix-library "/A/" (capitalize query) ".html")))
    (browse-url url)))

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
(if (and
     (member '("Wikipedia_Local" . "http://127.0.0.1:8000/wikipedia_zh_all_2015-11/A/%s.html") org-link-abbrev-alist)
     (assoc "Wikipedia_Local" org-link-abbrev-alist))
    
    (setq org-link-abbrev-alist
          (cons '("Wikipedia_Local" . "http://127.0.0.1:8000/wikipedia_zh_all_2015-11/A/%s.html") org-link-abbrev-alist))
  )



(provide 'kiwix)

;;; kiwix.el ends here
