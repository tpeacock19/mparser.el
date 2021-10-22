;;; mparser.el --- mercury-parser configuration -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Trey Peacock
;;
;; Author: Trey Peacock <http://github/tpeacock19>
;; Maintainer: Trey Peacock <git@treypeacock.com>
;; Created: October 14, 2021
;; Modified: October 14, 2021
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/tpeacock19/mparser
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

(provide 'mparser)
;;; mparser.el ends here
