;;; addressbar.el --- Handy addressbar for text browsing -*- lexical-binding: t -*-

;; Author: KURASHIKI Satoru <lurdan@gmail.com>
;; Version: 0
;; Package-Requires: ((emacs "24.5"))
;; Homepage: https://github.com/lurdan/emacs-addressbar
;; Keywords: hypermedia
;; Prefix: addressbar

;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; TBD

;;; Code:

(require 'eww)
(require 'subr-x)
(require 'cl-lib)
(require 'thingatpt)

;; custom variables

(defgroup addressbar nil
  "Addressbar for text browsing"
  :group 'web
  :prefix "addressbar-")

(defcustom addressbar-persistent-history-directory user-emacs-directory
  "Directory where history files will be stored."
  :group 'addressbar
  :type 'directory)

(defcustom addressbar-ignore-url-regexp "\\(://duckduckgo\\.com/\\|google\\.com/search\\)"
  "Regexp of URLs which won't list or save as history."
  :group 'addressbar
  :type 'regexp)

(defcustom addressbar-cleanup-threshold 10000
  "Histories are automatically pruned when exceeding this value, 0 means histories won't be housekept."
  :group 'addressbar
  :type 'integer)

(defcustom addressbar-search-command-alist '(
                                      ("g" . "https://google.com/search?q=")
                                      ("weblio" . "https://ejje.weblio.jp/content/"))
  "You can define specific search command using this associated list."
  :group 'addressbar
  :type '(alist :key-type string :value-type string))

(defcustom addressbar-display-url-max-length nil
  "When non-0, URLs are cut off at this length to display."
  :group 'addressbar
  :type 'integer)

(defcustom addressbar-display-icons t
  "When non-nil, it try to display emoji icons."
  :group 'addressbar
  :type 'boolean)

(defcustom addressbar-time-format "%m-%d %H:%M"
  "Format of the time displayed."
  :group 'addressbar
  :type 'string)

;; internal variables
(defvar addressbar--entries (make-hash-table :test #'equal)
  "Variable which holds addressbar histories.")

(defvar addressbar--debug nil
  "When non-nil, internal debug messages will be logged to *Messages* buffer.")

(defvar addressbar--startup t
  "When t, collect histories of browser buffers as initial setup.")

;; internal functions

(defun addressbar--log (msg param)
  "Put debug log to *Messages* buffer."
  (if addressbar--debug
      (message "DEBUG: %s %s" msg param)))

(defun addressbar--cleanup ()
  "Delete oldest entries from candidates"
  (if (< addressbar-cleanup-threshold (hash-table-count addressbar--entries))
      (dolist (entry (cl-subseq (sort (hash-table-keys addressbar--entries) 'addressbar--have-newer-timestamp)
                                (round (* addressbar-cleanup-threshold .9))))
        (unless (eq (addressbar--get-type entry) :b)
          (addressbar--log "cleanup:" entry)
          (remhash entry addressbar--entries)))))

(defun addressbar--have-newer-timestamp (x y)
  "Return t if each history's timestamp, is less than the next one."
  (if (< (addressbar--get-time x)
         (addressbar--get-time y))
      nil t))

(defun addressbar--save-persistent-history ()
  "Save current history to disk."
  (addressbar--cleanup)
  (with-temp-file (expand-file-name "addressbar-history" addressbar-persistent-history-directory)
    (insert ";; Auto-generated file; don't edit\n")
    (insert (prin1-to-string addressbar--entries))))

(defun addressbar--load-persistent-history ()
  "Reload saved history."
  (let ((file (expand-file-name "addressbar-history" addressbar-persistent-history-directory)))
    (if (file-exists-p file)
        ;; need error handling?
        (setq addressbar--entries (with-temp-buffer
                                    (insert-file-contents file)
                                    (read (current-buffer)))))))

(defun addressbar--get-type (entry)
  "Getter of history metadata. Returns type symbol :b(ookmark) or :h(istory)."
  (nth 0 (gethash entry addressbar--entries)))

(defun addressbar--get-title (entry)
  "Getter of history metadata. Returns page title."
  (nth 1 (gethash entry addressbar--entries)))

(defun addressbar--get-time (entry)
  "Getter of history metadata. Returns last timestamp."
  (nth 2 (gethash entry addressbar--entries)))

(defun addressbar--add-entry (type plist)
  "Setter of history data."
  (let ((url (plist-get plist :url))
        (title (plist-get plist :title))
        (time (plist-get plist :time)))
    (unless (string-match-p addressbar-ignore-url-regexp url)
      (puthash url (list (if (eq (addressbar--get-type url) :b) :b type)
                         title
                         (time-to-seconds (if time
                                              (decode-time (parse-time-string time))
                                            (current-time))))
               addressbar--entries)
      (addressbar--save-persistent-history))))

;; eww related functions
(defun addressbar--eww-load-bookmark ()
  "Convert `eww' bookmark into history data."
  (eww-read-bookmarks)
  (dolist (bookmark eww-bookmarks)
    (unless (gethash (plist-get bookmark :url) addressbar--entries)
      (addressbar--add-entry :b bookmark))))

(defun addressbar--eww-add-current-history ()
  "Add or update history data with current browsing page."
  (addressbar--add-entry :h eww-data))
(add-hook 'eww-after-render-hook 'addressbar--eww-add-current-history)

(defun addressbar--eww-load-buffer-history (buf)
  "Convert history of given `eww' buffer into history data."
  (with-current-buffer buf
    (when (derived-mode-p 'eww-mode)
      (or (null eww-history)
          (let (his url title format start)
            (dolist (his eww-history)
              (addressbar--add-entry :h his))))
      (addressbar--eww-add-current-history))))

(defun addressbar--buffer-existp (entry)
  "Return buffer which renders specified url, if exists."
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (if (derived-mode-p 'eww-mode)
          (if (string-equal (plist-get eww-data :url) entry)
              (return buf)))
      (if (derived-mode-p 'w3m-mode)
          (if (string-equal w3m-current-url entry)
              (return buf))))))

;; w3m related functions
(when (locate-library "w3m")
  (require 'w3m)

  (defun addressbar--w3m-add-entry (type url title)
    "Add-entry wrapper for `w3m'."
    (let (plist)
      (setq plist (plist-put plist :url url))
      (setq plist (plist-put plist :title title))
      (addressbar--add-entry type plist)))

  (defun addressbar--w3m-load-buffer-history (buf)
    "Convert history of given `w3m' buffer into history data."
    (with-current-buffer buf
      (when (derived-mode-p 'w3m-mode)
        (dolist (his w3m-history-flat)
          (let ((url (car his))
                (title (cdadr his))
                plist)
            (unless (string-match "^about" url)
              (addressbar--w3m-add-entry :h url title)))))))

  (defun addressbar--w3m-add-current-history (dummy)
    "Add or update addressbar candidate with current browsing page."
    (addressbar--w3m-add-entry :h w3m-current-url w3m-current-title))
  (add-hook 'w3m-display-hook 'addressbar--w3m-add-current-history))

(defun addressbar--update-entries ()
  "Collect entries of history hash. \
Once in startup, it also search history of `eww' buffers."
  (addressbar--load-persistent-history)
  (addressbar--eww-load-bookmark)
  (when addressbar--startup   ;; should be run only once
    (dolist (buf (buffer-list))
      (if (featurep 'w3m) (addressbar--w3m-load-buffer-history buf))
      (addressbar--eww-load-buffer-history buf))
    (setq addressbar--startup nil)))

(defun addressbar-list-candidates ()
  "Enumerate history, bookmarks, and browsing buffers."
  (addressbar--update-entries)
  (hash-table-keys addressbar--entries))

;; functions for entry manipulation

(defun addressbar-copy-link-org (entry)
  "Copy selected URL as `org-mode' style format."
  (let ((title (addressbar--get-title entry)))
    (kill-new (format "[[%s][%s]]" entry title))))

(defun addressbar-delete-entry (entry)
  "Delete selected url from history candidates. \
If bookmarked with `eww', also delete it."
  (when (eq (addressbar--get-type entry) :b)
    (let (bm)
      (dolist (bookmark eww-bookmarks)
        (if (not (equal entry (plist-get bookmark :url)))
            (push bookmark bm)))
      (setq eww-bookmarks bm)
      (eww-write-bookmarks)))
  (remhash entry addressbar--entries)
  (addressbar--save-persistent-history))

(defun addressbar--thing-at-point ()
  "Return thing-at-point string. It requires prefix-arg."
  (if current-prefix-arg
      (or (thing-at-point 'url t)
          (thing-at-point 'filename t)
          (thing-at-point 'symbol t)
          (thing-at-point 'word t))))

(defun addressbar-open (select)
  "Open selected url, or switch buffer if it is already opened."
  (let ((buf (addressbar--buffer-existp select))
        (url (replace-regexp-in-string "\\$$" "" select)))
    (if buf
        (switch-to-buffer buf)
      (let* ((input (split-string url))
             (keyword (assoc (car input) addressbar-search-command-alist)))
        (if keyword
            (browse-url (concat (cdr keyword) (substring url (length (car keyword)))))
          (browse-url url))))))

;; mimics eww's function
(defun addressbar-add-bookmark (entry)
  "Add selected url to `eww' bookmark."
  (interactive)
  (eww-read-bookmarks)
  (dolist (bookmark eww-bookmarks)
    (when (equal entry (plist-get bookmark :url))
      (user-error "Already bookmarked")))
  (when (y-or-n-p "Bookmark this page?")
    (let ((title (replace-regexp-in-string "[\n\t\r]" " "
                                           (addressbar--get-title entry))))
      (setq title (replace-regexp-in-string "\\` +\\| +\\'" "" title))
      (push (list :url entry
                  :title title
                  :time (current-time-string))
            eww-bookmarks)
      (message "Bookmarked %s (%s)" entry title))
    (eww-write-bookmarks)))

;; functions for display data

(defun addressbar--column-width (type)
  (let* ((max (frame-width))
         (url (or addressbar-display-url-max-length
                  (if (> (/ max 3) 120) 120 (/ max 3)))))
    (pcase type
      (:url url)
      (:title (- (/ max 2) url)))))

(defun addressbar--display-url (entry)
  "Return URL modified for selection display, cutting off too long and prefix site icon."
  (let ((max (addressbar--column-width :url))
        (icon (and addressbar-display-icons (featurep 'all-the-icons) (all-the-icons-icon-for-url entry)))
        (len (length entry)))
    (concat icon (substring entry 0 (if (> len max)
                                        max len)))))

(defun addressbar--set-icon (e u c)
  "Display icon with predefined fallbacks."
  (cond ((and addressbar-display-icons (featurep 'all-the-icons))
         (all-the-icons-material e :height .9))
        ((and addressbar-display-icons (char-displayable-p ?u))
         u)
        (t c)))

(defun addressbar--display-type (entry)
  "Return entry type signature character, each of t(ab), b(ookmark), h(istory)"
  (if (addressbar--buffer-existp entry)
      (addressbar--set-icon "tab" "📑" "t")
  (pcase (addressbar--get-type entry)
    (:b (addressbar--set-icon "bookmark" "🔖" "b"))
    (:h (addressbar--set-icon "history" "📜" "h")))))

(defun addressbar--display-time (entry)
  "Return formatted time string. see `addressbar-time-format'."
  (format-time-string addressbar-time-format (seconds-to-time (addressbar--get-time entry))))

;; functions for interactive interface

(defun addressbar-kill-all-browser-buffers ()
  "Close all tabs."
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (if (derived-mode-p 'eww-mode 'w3m-mode)
          (kill-buffer buf)))))

;;;###autoload
(defun addressbar ()
  "Handy addressbar function for emacs browser."
 (interactive)
  (addressbar-open (completing-read "Browse: "
                                    (sort (addressbar-list-candidates) 'addressbar--have-newer-timestamp)
                                    nil nil
                                    (addressbar--thing-at-point))))

;;;###autoload
(defun ido-addressbar ()
    "`ido' wrapper of `addressbar'."
    (interactive)
    (let ((urls (mapcar (lambda (f)
                           (cons (addressbar--display-url f) f))
                         (sort (addressbar-list-candidates) 'addressbar--have-newer-timestamp))))
      (let ((select (ido-completing-read "Browse: " (mapcar #'car urls) nil nil (addressbar--thing-at-point))))
        (addressbar-open (assoc-default select urls)))))

(provide 'addressbar)

;;; addressbar.el ends here
