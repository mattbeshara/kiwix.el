;;; org-kiwix.el --- Org Mode link support -*- lexical-binding: t; -*-

;;; Time-stamp: <2020-05-04 07:04:56 stardiviner>

;;; Commentary:

;;; Support Org-mode
;;
;; - [[wikipedia:(library):query]]
;; - [[wikipedia:query]]
;;
;; links:
;; - wikipedia:(zh):%E7%A6%85%E5%AE%97
;; - wikipedia:(en):linux
;; - wikipedia:linux
;;
;; - parameter `link' will be (en):linux" or linux".
;;
;; elisp regexp: "\\(?:(\\(.*\\)):\\)?\\([^] \n\t\r]*\\)"
;; - non capturing group (\(?:...\)) for optional library
;; - group 1: library (en or zh)
;; - group 2: link? (match everything but ], space, tab, carriage return, linefeed by using [^] \n\t\r]*)
;; for open wiki search query with local application database.

;;; Code:

(require 'kiwix)

(defun chinese-string-p (string)
  "Return t if STRING is a Chinese string."
  (if (string-match (format "\\cC\\{%s\\}" (length string)) string)
      t
    nil))

(defun kiwix-org-get-library (link)
  "Get library from Org-mode `LINK'."
  (cond
   ((chinese-string-p link)
    (kiwix-select-library "zh"))
   ((string-match-p "[a-zA-Z\ ]+" link)
    ;; convert between libraries full name and abbrev.
    (kiwix-select-library "en"))
   (t (kiwix-select-library))))

;;;###autoload
(defun org-wikipedia-open-link (link)
  "Open LINK in external Wikipedia program."
  ;; The regexp: (library):query
  ;; - query : should not exclude space
  ;; match link spec: "(library):query" with regexp "([^).]*):?:.*"
  ;; (string-match "\\(?:(\\(.*\\)):\\)?\\([^]\n\t\r]*\\)"  link)
  (string-match "(\\([^)].*\\)):\\(.*\\)" link)
  (let* ((library (kiwix-org-get-library link))
         (query (cond
                 ((chinese-string-p link) link)
                 ((string-match-p "(\\([^)].*\\)):\\(.*\\)" link)
                  (match-string 2 link))
                 (t link)))
         (url (concat
               kiwix-server-url
               "/" library "/A/"
               ;; query need to be convert to URL encoding: "禅宗" https://zh.wikipedia.org/wiki/%E7%A6%85%E5%AE%97
               (url-encode-url
                ;; convert space to underline: "Beta distribution" "Beta_distribution"
                (replace-regexp-in-string
                 " " "_"
                 ;; only capitalize the first word. like: "meta-circular interpreter" -> "Meta-circular interpreter"
                 (kiwix-capitalize-first query)
                 nil nil))
               ".html")))
    ;; (prin1 (format "library: %s, query: %s, url: %s" library query url))
    (browse-url url)))

;;;###autoload
(defun org-wikipedia-export-link (link description format)
  "Export the Wikipedia LINK with DESCRIPTION for FORMAT from Org files."
  (when (string-match "\\(?:(\\(.*\\)):\\)?\\([^] \n\t\r]*\\)" link)
    (let* ((library (kiwix-org-get-library link))
           (query (url-encode-url (or (match-string 2 link) description)))
           ;; "http://en.wikipedia.org/wiki/Linux"
           ;;         --
           ;;          ^- library: en, zh
           (path (concat "http://" library ".wikipedia.org/wiki/" query))
           (desc (or (match-string 2 link) description)))
      (when (stringp path)
        (cond
         ((eq format 'html) (format "<a href=\"%s\">%s</a>" path desc))
         ((eq format 'latex) (format "\\href{%s}{%s}" path desc))
         (t path))))))

;;;###autoload
(defun org-wikipedia-store-link ()
  "Store a link to a Wikipedia link."
  ;; [C-c o C-l l] `org-store-link'
  ;; remove those interactive functions. use normal function instead.
  (when (eq major-mode 'wiki-mode)
    (let* ((query (read-string "Wikipedia Query with Kiwix: "))
           (library (kiwix-select-library))
           (link (concat "wikipedia:" "(" library "):" query)))
      (org-store-link-props :type "wikipedia"
                            :link link
                            :description query)
      link)))

(defun org-wikipedia-complete-link (&optional arg)
  "Use kiwix AJAX request to provide available completion keywords."
  (let* ((input (or arg (read-from-minibuffer "Search keyword: ")))
         (keywords (kiwix-ajax-search-hints input)))
    (concat "wikipedia:"
            (completing-read "Available keywords: " keywords))))

;;;###autoload
(with-eval-after-load 'org
  (org-link-set-parameters "wikipedia" ; NOTE: use `wikipedia' for future backend changing.
                           :follow #'org-wikipedia-open-link
                           :store #'org-wikipedia-store-link
                           :export #'org-wikipedia-export-link
                           :complete #'org-wikipedia-complete-link)
  (add-hook 'org-store-link-functions 'org-wikipedia-store-link t))



(provide 'org-kiwix)

;;; org-kiwix.el ends here
