;;; mparser-eww.el --- description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Trey Peacock
;;
;; Author: Trey Peacock <http://github/tpeacock19>
;; Maintainer: Trey Peacock <git@treypeacock.com>
;; Created: October 22, 2021
;; Modified: October 22, 2021
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/tpeacock19/mparser-eww
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
;;  description
;;
;;; Code:

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

(provide 'mparser-eww)
;;; mparser-eww.el ends here
