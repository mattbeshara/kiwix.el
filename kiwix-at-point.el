;;; kiwix-at-point.el --- Kiwix client for Emacs
;;; -*- coding: utf-8 -*-

;;; Commentary:



;;; Code:

(defgroup kiwix-at-point nil
  "Search in Kiwix for text at point."
  :group 'external)

(defcustom kiwix-at-point-library-alist
  ;; e.g. "~/.www.kiwix.org/kiwix/8ip89lik.default/data/index/wikinews_en_all_2015-11.zim.idx"
  '((wikipedia . "")
    (wiktionary . "")
    (wikinews . "")
    (wikibooks . "")
    (wikiquote . "")
    (wikisource . "")
    (wikiversity . "")
    (wikimed . "") ; "WikiMed Medical Encyclopedia" "Wikispecies"
    )
  "Alist which contains available kiwix libraries."
  :type '(repeat 'string)
  :group 'kiwix-at-point)

(defvar kiwix-at-point-library nil
  "Variable used to specify the library for the current search.")
(make-variable-buffer-local 'kiwix-at-point-library)

(defvar kiwix-at-point--library-history nil)

(defun kiwix-at-point-get-library ()
  "Guess which library suit to the current search."
  (or kiwix-at-point-library
      kiwix-at-point--library-history))

(defun kiwix-at-point-maybe-specify-library (search-string)
  "Prefix SEARCH-STRING with the guessed library, if any."
  (let ((library (kiwix-at-point-get-library)))
    (concat (when library
              (concat library ":"))
            search-string)))

(defun kiwix-at-point-get-idx ()
  "Get the idx file path."
  ;; TODO:
  )

(defun kiwix-at-point-run-search (search)
  (if (executable-find "kiwix")
      ;; TODO:
      (start-process "Kiwix" nil "kiwix-search" (kiwix-at-point-get-idx) search)
    (message "Kiwix wasn't found, install it first http://www.kiwix.org")))

;;;###autoload
(defun kiwix-at-point (&optional edit-search)
  "Search for the word at point in Kiwix."
  (interactive "P")
  (let* ((thing (if mark-active
                    (buffer-substring (region-beginning) (region-end))
                  (thing-at-point 'symbol)))
         (search (kiwix-at-point-maybe-specify-library thing)))
    (kiwix-at-point-run-search
     (if (or edit-search (null thing))
         (read-string "Kiwix search: " search)
       search))))

(defun kiwix-at-point--library-candidates ()
  kiwix-at-point-library-alist)

(defun kiwix-at-point--set-library-prompt ()
  (let ((default-library (kiwix-at-point-get-library)))
    (format "Kiwix library%s: "
            (if default-library
                (format "[Default: %s]" default-library)
              ""))))

;;;###autoload
(defun kiwix-at-point-set-library ()
  "Set current search's library."
  (interactive)
  (let ((minibuffer-local-completion-map
         (copy-keymap minibuffer-local-completion-map)))
    (define-key minibuffer-local-completion-map (kbd "SPC") nil)
    (setq-local kiwix-at-point-library
                (completing-read (kiwix-at-point--set-library-prompt)
                                 (kiwix-at-point--library-candidates) nil nil nil
                                 'kiwix-at-point--library-history (kiwix-at-point-get-library)))))

;;;###autoload
(defun kiwix-at-point-search (&optional edit-search)
  "Prompt and search in Kiwix."
  (interactive "P")
  (let ((search (kiwix-at-point-maybe-specify-library "")))
    (kiwix-at-point-run-search
     (read-string "Kiwix search: " search))))


(provide 'kiwix-at-point)

;;; kiwix-at-point.el ends here
