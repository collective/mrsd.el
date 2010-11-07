;;; mrsd.el --- Emacs integration for mrs.developer.

;; Copyright (C) 2010 Jonas Baumann

;; Author: Jonas Baumann, http://github.com/jone
;; URL: http://github.com/collective/mrsd.el
;; Version: 0.1
;; Created: 17 October 2010
;; Keywords: emacs mrs developer

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(require 'json)


(defun mrsd-reload ()
  (interactive)
  (let ((data (shell-command-to-string "mrsd reload --list-instances")))
    (if (= 0 (length data))
        (message "No instance found. Add mrs.developer as buildout extension and re-run buildout.")
      (let ((instances (coerce (json-read-from-string data) 'list)))
        (if (= 1 (length instances))
            (shell-command (concat "mrsd reload --instance " (nth 0 instances)))
          (shell-command (concat "mrsd reload --instance "
                                 (ido-completing-read "Instance: " instances))))))))


(defun mrsd-i18n-build ()
  "Run i18ndude build pot using mrs.developer"
  (interactive)
  (shell-command "mrsd i18ndude --build"))


(defun mrsd-i18n-sync ()
  "Run i18ndude sync using mrsd.developer"
  (interactive)
  (let ((output (shell-command-to-string "mrsd i18ndude --sync")))
    (if (string-match "\/.*.po" output)
        (find-file (match-string 0 output)))
    (message output)))


(setq mrsd-mode-map
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "C-c m r") 'mrsd-reload)
        (define-key map (kbd "C-c m i") 'mrsd-i18n-build)
        (define-key map (kbd "C-c m C-i") 'mrsd-i18n-sync)
        map
        ))


(provide 'mrsd)
