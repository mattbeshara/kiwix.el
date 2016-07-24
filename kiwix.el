;;; kiwix.el --- Kiwix interface and support.
;;; -*- coding: utf-8 -*-

;; Author: stardiviner <numbchild@gmail.com>
;; Maintainer: stardiviner <numbchild@gmail.com>
;; Keywords: kiwix wikipedia
;; URL: https://github.com/stardiviner/kiwix.el
;; Created: 23th July 2016
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.3"))

;;; Commentary:

;;; This currently only works for Linux, not tested for Mac OS X and Windows.

;;; Usage:
;;
;; [M-x kiwix-launch-server] to launch Kiwix server.
;; [M-x kiwix-at-point] to search the word under point or the region selected string.


;; ;;; Support Org-mode
;; ;; [[wiki:]]
;; ;; for open wiki search query with local application database.
;; (defalias 'org-wiki-link-open 'kiwix-query)

;; (if kiwix-support-org-mode-link-type
;;     (org-add-link-type "wiki" 'org-wiki-link-open))

;; ;; [[Wikipedia_Local:]]
;; (if (and
;;      kiwix-support-org-mode-link-abbrev
;;      (member '("Wikipedia_Local" . "http://127.0.0.1:8000/wikipedia_zh_all_2015-11/A/%s.html") org-link-abbrev-alist)
;;      (assoc "Wikipedia_Local" org-link-abbrev-alist))

;;     (setq org-link-abbrev-alist
;;           (cons '("Wikipedia_Local" . "http://127.0.0.1:8000/wikipedia_zh_all_2015-11/A/%s.html") org-link-abbrev-alist))
;;   )

;;; Code:


(defgroup kiwix nil
  "Kiwix customization options.")

(defcustom kiwix-server-url "http://127.0.0.1:8000/"
  "Specify Kiwix server URL."
  :type 'string
  :group 'kiwix)

(defcustom kiwix-server-command "/usr/lib/kiwix/bin/kiwix-serve "
  "Specify kiwix server command."
  :type 'string
  :group 'kiwix)

(defcustom kiwix-default-data-profile-name "8ip89lik.default"
  "Specify the default Kiwix data profile path."
  :type 'string
  :group 'kiwix)

(defcustom kiwix-default-data-path (shell-quote-argument (concat (getenv "HOME") "/.www.kiwix.org/kiwix/" kiwix-default-data-profile-name))
  "Specify the default Kiwix data path."
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

(defcustom kiwix-support-org-mode-link-type t
  "Add support for Org-mode Kiwix link type."
  :type 'boolean
  :group 'kiwix)

(defcustom kiwix-support-org-mode-link-abbrev t
  "Add support for Org-mode Kiwix link abbrev."
  :type 'boolean
  :group 'kiwix)

(defvar kiwix-libraries (directory-files (concat kiwix-default-data-path "/data/content/") nil "\.zim")
  "A list of Kiwix libraries.")

;; launch Kiwix server
;;;###autoload
(defun kiwix-launch-server ()
  "Launch Kiwix server."
  (interactive)
  
  (let ((library "--library ")
        (port (concat "--port=" kiwix-server-port " "))
        (daemon "--daemon ")
        (library-path (concat kiwix-default-data-path "/data/library/library.xml"))
        )
    (async-shell-command
     (concat kiwix-server-command library port daemon library-path))))


(defun kiwix-query (query &optional library)
  "Search `QUERY' in `LIBRARY' with Kiwix."
  (let* ((kiwix-library (if library
                            library
                          kiwix-default-library))
         (url (concat kiwix-server-url kiwix-library "/A/" (capitalize query) ".html")))
    (browse-url url)))

;;;###autoload
(defun kiwix-at-point (&optional interactively)
  "Search for the symbol at point with `kiwix-query'.

Or When prefix argument `INTERACTIVELY' specified, then prompt
for query string and library interactively."
  (interactive "P")
  (let* ((query-string (if interactively
                           (read-string "Kiwix Search: "
                                        (if mark-active
                                            (buffer-substring
                                             (region-beginning) (region-end))
                                          (thing-at-point 'symbol)))))
         (library (when interactively
                    (completing-read "Kiwix Library: "
                                     (mapcar #'(lambda (var)
                                                 (replace-regexp-in-string "\.zim" "" var))
                                             kiwix-libraries)))))
    (kiwix-query query-string library)))


(provide 'kiwix)

;;; kiwix.el ends here
