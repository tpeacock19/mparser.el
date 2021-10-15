;;; mparser.el --- mercury-parser configuration -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Trey Peacock
;;
;; Author: Trey Peacock <http://github/Symbol’s value as variable is void: baal-github-login-name>
;; Maintainer: Trey Peacock <Symbol’s value as variable is void: baal-github-mail-address>
;; Created: October 14, 2021
;; Modified: October 14, 2021
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/Symbol’s value as variable is void: baal-github-login-name/mparser
;; Package-Requires: ((emacs 28.0.60) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;; This file is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation; either version 3, or (at your option) any
;; later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;;  mercury-parser configuration
;;
;;; Code:

(require 'json)

(defun mparser--retrieve-url (url callback &rest cbargs)
  (let* ((json-object-type 'alist)
         (json-array-type 'list)
         (json-key-type 'symbol)
         (buffer (generate-new-buffer " *mercury retrieve*"))
         (error-buffer (generate-new-buffer " *mercury error*"))
         (code (call-process "mercury-parser" nil (cons buffer error-buffer)
                             nil url)))
    (if (= code 0)
        (with-current-buffer buffer
          (goto-char (point-min))
          (funcall callback (json-read) cbargs))
      (with-current-buffer error-buffer
        (user-error "Unable to run parser: %s"
                    (buffer-substring (point-min) (point-max)))))))

;;; * EWW

(declare-function eww-readable "eww")
(declare-function eww-save-history "eww")
(declare-function eww-display-html "eww")
(declare-function eww--preprocess-html "eww")
(declare-function eww-score-readability "eww")
(declare-function eww-highest-readability "eww")
(declare-function eww-update-header-line-format "eww")
(declare-function parse-iso8601-time-string "parse-time")

(defun mparser-eww--parse-json (json &rest _)
  (let* ((title (format "<h2>%s</h2>\n" (alist-get 'title json)))
         (author (alist-get 'author json))
         (date1 (alist-get 'date_published json))
         (date (when date1
                 (format-time-string "%a, %b %d, %Y at %H:%M:%S %Z"
                                     (parse-iso8601-time-string date1))))
         (byline (concat (when author (format " <p>By %s</p>" author))
                         (when date (format " <p>On %s</p>" date))
                         "<hr>"))
         (image (format "<img src=\"%s\">\n" (alist-get 'lead_image_url json)))
         (err (alist-get 'error json))
         (content (alist-get 'content json)))
    (if err
        (user-error (format "%s" err))
      (unless (string= content "<div></div>")
        (with-temp-buffer
          (insert title (when image image) byline content)
          (condition-case nil
              (decode-coding-region (point-min) (point-max) 'utf-8)
            (coding-system-error nil))
          (eww--preprocess-html (point-min) (point-max))
          (libxml-parse-html-region (point-min) (point-max)))))))

(defun mparser-eww-readable ()
  "View the main \"readable\" parts of the current web page.
This command uses heuristics to find the parts of the web page that
contains the main textual portion, leaving out navigation menus and
the like."
  (interactive nil eww-mode)
  (if-let* ((old-data eww-data)
            (base (plist-get old-data :url))
            (dom (mparser--retrieve-url base #'mparser-eww--parse-json)))
      (progn
        (eww-score-readability dom)
        (eww-save-history)
        (eww-display-html nil nil
                          (list 'base (list (cons 'href base))
                                (eww-highest-readability dom))
                          nil (current-buffer))
        (dolist (elem '(:source :url :title :next :previous :up))
          (plist-put eww-data elem (plist-get old-data elem)))
        ;; update for this commit
        ;; 171de3eee459ed64388a8ced7d07fa031ea025a6 in emacs29
        (if (fboundp #'eww--after-page-change)
            (eww--after-page-change)
          (eww-update-header-line-format)))
    (funcall #'eww-readable)))

;;; * Elfeed

(declare-function elfeed-ref "elfeed-db")
(declare-function elfeed-meta "elfeed-db")
(declare-function elfeed-deref "elfeed-db")
(declare-function elfeed-tagged-p "elfeed-db")
(declare-function elfeed-entry-link "elfeed-db")
(declare-function elfeed-entry-content "elfeed-db")
(declare-function elfeed-show-refresh "elfeed-show")
(declare-function elfeed-search-selected "elfeed-search")
(declare-function elfeed-search-show-entry "elfeed-search")

(defun mparser-elfeed--parse-json (json args)
  "Elfeed JSON parser for `mparser-parse'.
Include ARGS for elfeed entry and excerpt options."
  (let* ((err (alist-get 'error json))
         (content (alist-get 'content json))
         (author (alist-get 'author json))
         (date1 (alist-get 'date_published json))
         (date (when date1
                 (format-time-string "%a, %b %d, %Y at %H:%M:%S %Z"
                                     (parse-iso8601-time-string date1))))
         (byline (concat (when author (format " <p>By %s</p>" author))
                         (when date (format " <p>On %s</p>" date))
                         "<hr>"))
         (image (format "<img src=\"%s\">\n" (alist-get 'lead_image_url json)))
         (entry (car args))
         (keepExcerpt (cadr args))
         article header)
    (if err
        (user-error (format "%s" err))
      (if image
          (setq header (concat image byline))
        (setq header byline))
      (setq article (format "%s\n%s" header content))
      (when keepExcerpt
        (setq article (concat (elfeed-deref (elfeed-entry-content entry)) "<br><br>---<br>" article)))
      (when (string= content "<div></div>")
        (setq article nil))
      article)))

;; need to have to avoid `setf' errors in function below
(eval-when-compile (require 'elfeed))

(defun mparser-elfeed--readability-content (entry &optional keepExcerpt)
  "Replace entry content with readability article.
Some feeds (like heise.de) only provide a summary and not the full article.
This uses a python script to fetch the readable part of the original
article content.  Like in Firefox article view. If keepExcerpt is non nil,
keep the original excerpt above the article."
  (unless (elfeed-meta entry :readability)
    (when-let* ((url (elfeed-entry-link entry))
                (article (mparser--retrieve-url url #'mparser-elfeed--parse-json entry keepExcerpt)))
      (setf (elfeed-entry-content entry) (elfeed-ref article)))
    (setf (elfeed-meta entry :readability) t)))

(defun mparser-elfeed-show-readable ()
  "Convert `elfeed-show-entry' into readable format."
  (interactive)
  (mparser-elfeed--readability-content elfeed-show-entry)
  (elfeed-show-refresh))

(defun mparser-elfeed-search-readable (entry)
  "Download readable content from website and show entry in a buffer.
This command is like `elfeed-search-show-entry' but it first downloads the
readable website content if the feed is tagged 'readable'. If the feed is tagged
'excerptReadable' the excerpt is kept above the readable content."
  (interactive (list (elfeed-search-selected :ignore-region)))
  (cond ((elfeed-tagged-p 'excerptReadable entry) (mparser-elfeed--readability-content entry t))
        ((elfeed-tagged-p 'readable entry) (mparser-elfeed--readability-content entry nil)))
  (elfeed-search-show-entry entry))

(provide 'mparser)
;;; mparser.el ends here
