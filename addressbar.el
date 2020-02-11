(require 'eww)
(require 'subr-x)
(require 'cl)

(defgroup addressbar nil
  "Addressbar for Emacs"
  :group 'web
  :prefix "addressbar-")

(defcustom addressbar-persistent-history-directory user-emacs-directory
  "Directory where history files will be stored."
  :group 'addressbar
  :type 'directory)

(defcustom addressbar-ignore-url-regexp "\\(://duckduckgo\\.com/\\|google\\.com/search\\)"
  "URL match this regexp won't list or save as history."
  )

(defcustom addressbar-cleanup-threshold 0
  ""
  )

(defcustom addressbar-copy-org-link nil
  ""
  )

(defvar addressbar--entries (make-hash-table :test #'equal)
  ""
  )

(defun addressbar-copy-link (entry)
  ""
  ;; addressbar-copy-org-link
  )

(defun addressbar--cleanup ()
  "TODO: delete oldest entries from candidates"
  )

(defun addressbar--have-newer-timestamp (x y)
  "compare entry's timestamp."
  (if (< (addressbar--get-time x)
         (addressbar--get-time y))
      nil t))

(defun addressbar--save-persistent-history ()
  "save current addressbar history into disk"
  (if (< addressbar-cleanup-threshold (hash-table-count addressbar--entries))
      (addressbar--cleanup))
  (with-temp-file (expand-file-name "addresbar-history" addressbar-persistent-history-directory)
    (insert ";; Auto-generated file; don't edit\n")
    (insert (prin1-to-string addressbar--entries))))

(defun addressbar--load-persistent-history ()
  "reload saved history"
  (let ((file (expand-file-name "addresbar-history" addressbar-persistent-history-directory)))
    (if (file-exists-p file)
        ;; need error handling?
        (setq addressbar--entries (with-temp-buffer
                                    (insert-file-contents file)
                                    (read (current-buffer)))))))

(defun addressbar--get-type (entry)
  (nth 0 (gethash entry addressbar--entries)))

(defun addressbar--get-title (entry)
  (nth 1 (gethash entry addressbar--entries)))

(defun addressbar--get-time (entry)
  (nth 2 (gethash entry addressbar--entries)))

(defun addressbar--add-entry (type plist)
  "Setter of addressbar candidates."
  (let ((url (plist-get plist :url))
        (title (plist-get plist :title))
        (time (plist-get plist :time)))
    (unless (string-match-p addressbar-ignore-url-regexp url)
      (puthash url (list type
                         title
                         (time-to-seconds (if time
                                              (decode-time (parse-time-string time))
                                            (current-time))))
               addressbar--entries)
      (addressbar--save-persistent-history)
      )))

;; eww related functions
(defun addressbar--eww-load-bookmark ()
  "Convert bookmark of eww into addressbar candidates."
  (eww-read-bookmarks)
  (dolist (bookmark eww-bookmarks)
    (unless (gethash (plist-get bookmark :url) addressbar--entries)
      (addressbar--add-entry :bookmark bookmark)
      )))

(defun addressbar--eww-add-current-history ()
  "Add or update addressbar candidate with current browsing page."
  (addressbar--add-entry :history eww-data)
  )
(add-hook 'eww-after-render-hook 'addressbar--eww-add-current-history)

(defun addressbar--eww-load-buffer-history (buf)
  "Convert history of given eww buffer into addressbar candidates."
  (with-current-buffer buf
    (when (derived-mode-p 'eww-mode)
      (or (null eww-history)
          (let (his url title format start)
            (dolist (his eww-history)
              (addressbar--add-entry :history his)
              )))
      (addressbar--eww-add-current-history)
      )))

(defun addressbar--eww-buffer-existp (entry)
  "Return eww buffer which renders specified url, if exists."
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (if (derived-mode-p 'eww-mode)
          (if (string-equal (plist-get eww-data :url) entry)
              (return buf))
        ))))

(defvar addressbar--startup t)
(defun addressbar--update-entries ()
  "Collect entries of history hash. \
Once in startup, it also search history of eww buffers."
  (addressbar--load-persistent-history)
  (addressbar--eww-load-bookmark)
  (when addressbar--startup   ;; should be run only once
    (dolist (buf (buffer-list))
      (addressbar--eww-load-buffer-history buf))
    (setq addressbar--startup nil))
  )

(defun addressbar-list-candidates ()
  "Enumerate history, bookmarks, and browsing buffers."
  (addressbar--update-entries)
  (hash-table-keys addressbar--entries)
  )

(defun addressbar-delete-entry (entry)
  "Delete selected url from history candidates. \
If bookmarked with eww, also delete it."
  (when (eq (addressbar--get-type entry) :bookmark)
    (let (bm)
      (dolist (bookmark eww-bookmarks)
        (if (not (equal entry (plist-get bookmark :url)))
            (push bookmark bm)
          ))
      (setq eww-bookmarks bm)
      (eww-write-bookmarks)
      ))
  (remhash entry addressbar--entries)
  (addressbar--save-persistent-history)
  )

;; mimics eww's function
(defun addressbar-add-bookmark (entry)
  "Add selected url to eww's bookmark."
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
      (message "Bookmarked %s (%s)" entry title)
      )
    (eww-write-bookmarks)
    ))

(defun addressbar-open (select)
  "Open selected url, or switch buffer if it is already opened."
  (let ((buf (addressbar--buffer-existp select)))
    (if buf
        (switch-to-buffer buf)
      (cond ((string-match "g " select) (browse-url (concat eww-search-prefix select)))
            (t (browse-url select))
            ))
    ))

;;;###autoload
(defun addressbar ()
  "Handy addressbar function for emacs browser."
  (interactive)
  (addressbar-open (completing-read "Browse: " (sort (addressbar-list-candidates) 'addressbar--have-newer-timestamp))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (locate-library "ivy")
  (require 'ivy)

  (ivy-set-actions 'counsel-addressbar
                   '(("d" addressbar-delete-entry "Delete this URL")
                     ("o" brouse-url-generic "Open URL in generic-browser")
                     ("b" addressbar-add-bookmark "Add URL to eww bookmark")
                     ;;("c" addressbar-copy-link "Copy URL")
                     ))

;;;###autoload
  (defun counsel-addressbar ()
    "Onestop browsing manager."
    (interactive)
    (ivy-read "Browse: " (addressbar-list-candidates)
              :action 'addressbar-open
              :history 'counsel-addressbar-history
              :sort t
              ))

  (add-to-list 'ivy-sort-functions-alist
             '(counsel-addressbar . addressbar--have-newer-timestamp))

  (defun counsel-addressbar-display-type (entry)
    (pcase (addressbar--get-type entry)
      (:bookmark "b")
      (:history "h")
      ))

  (defun counsel-addressbar-display-icon (entry)
      (all-the-icons-xxxx "XXXX" :height .9))

  (defun counsel-addressbar-display-time (entry)
    (format-time-string "%Y-%m-%d %H:%M" (seconds-to-time (addressbar--get-time entry)))
    )

  (defun counsel-addressbar-display-url (entry)
    )

  (setq ivy-rich-display-transformers-list
        (plist-put ivy-rich-display-transformers-list
                   'counsel-addressbar
                   '(:columns
                     ((counsel-addressbar-display-type (:face success :width 1))
                      (counsel-addressbar-display-time (:width 16))
                      (ivy-rich-candidate (:width 0.7))
                      (addressbar--get-title (:width 0.1 :face font-lock-doc-face))
                      ))
                   ))
  (if ivy-rich-mode (ivy-rich-reload))

  ;; ignore eww buffers from buffer list
  ;; company-dabbrev-ignore-buffers
  )

(provide 'addressbar)
