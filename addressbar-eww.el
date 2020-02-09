(require 'eww)
(require 'subr-x)
(require 'cl)

(defgroup addressbar-eww nil
  "Addressbar for EWW"
  :group 'web
  :prefix "addressbar-eww-")

(defcustom addressbar-eww-persistent-history-directory user-emacs-directory
  "Directory where history files will be stored."
  :group 'addressbar-eww
  :type 'directory)

(defcustom addressbar-eww-ignore-url-regexp "\\(://duckduckgo\\.com/\\|google\\.com/search\\)"
  "URL match this regexp won't list or save as history."
  )

(defcustom addressbar-eww-cleanup-threshold 0
  ""
  )

(defcustom addressbar-eww-copy-org-link nil
  ""
  )

(defvar addressbar-eww--entries (make-hash-table :test #'equal)
  ""
  )

(defun addressbar-eww-copy-link (entry)
  ""
  ;; addressbar-eww-copy-org-link
  )

(defun addressbar-eww--cleanup ()
  "TODO: delete oldest entries from candidates"
  )

(defun addressbar-eww--have-newer-timestamp (x y)
  "compare entry's timestamp."
  (if (< (addressbar-eww--get-time x)
         (addressbar-eww--get-time y))
      nil t))

(defun addressbar-eww--save-persistent-history ()
  "save current addressbar history into disk"
  (if (< addressbar-eww-cleanup-threshold (hash-table-count addressbar-eww--entries))
      (addressbar-eww--cleanup))
  (with-temp-file (expand-file-name "addresbar-eww-history" addressbar-eww-persistent-history-directory)
    (insert ";; Auto-generated file; don't edit\n")
    (insert (prin1-to-string addressbar-eww--entries))))

(defun addressbar-eww--load-persistent-history ()
  "reload saved history"
  (let ((file (expand-file-name "addresbar-eww-history" addressbar-eww-persistent-history-directory)))
    (if (file-exists-p file)
        ;; need error handling?
        (setq addressbar-eww--entries (with-temp-buffer
                                        (insert-file-contents file)
                                        (read (current-buffer)))))))

(defun addressbar-eww--get-type (entry)
  (nth 0 (gethash entry addressbar-eww--entries)))

(defun addressbar-eww--get-title (entry)
  (nth 1 (gethash entry addressbar-eww--entries)))

(defun addressbar-eww--get-time (entry)
  (nth 2 (gethash entry addressbar-eww--entries)))

(defun addressbar-eww--add-entry (type plist)
  "Setter of addressbar candidates."
  (let ((url (plist-get plist :url))
        (title (plist-get plist :title))
        (time (plist-get plist :time)))
    (unless (string-match-p addressbar-eww-ignore-url-regexp url)
      (puthash url (list type
                         title
                         (time-to-seconds (if time
                                              (decode-time (parse-time-string time))
                                            (current-time))))
               addressbar-eww--entries)
      (addressbar-eww--save-persistent-history)
      )))

(defun addressbar-eww--load-bookmark ()
  "Convert bookmark of eww into addressbar candidates."
  (eww-read-bookmarks)
  (dolist (bookmark eww-bookmarks)
    (unless (gethash (plist-get bookmark :url) addressbar-eww--entries)
      (addressbar-eww--add-entry :bookmark bookmark)
      )))

(defun addressbar-eww--add-current-history ()
  "Add or update addressbar candidate with current browsing page."
  (addressbar-eww--add-entry :history eww-data)
  )
(add-hook 'eww-after-render-hook 'addressbar-eww--add-current-history)

(defun addressbar-eww--load-buffer-history (buf)
  "Convert history of given eww buffer into addressbar candidates."
  (with-current-buffer buf
    (when (derived-mode-p 'eww-mode)
      (or (null eww-history)
          (let (his url title format start)
            (dolist (his eww-history)
              (addressbar-eww--add-entry :history his)
              )))
      (addressbar-eww--add-current-history)
      )))

(defun addressbar-eww--buffer-existp (entry)
  "Return eww buffer which renders specified url, if exists."
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (if (derived-mode-p 'eww-mode)
          (if (string-equal (plist-get eww-data :url) entry)
              (return buf))
        ))))

(defvar addressbar-eww--startup t)
(defun addressbar-eww--update-entries ()
  "Collect entries of history hash. \
Once in startup, it also search history of eww buffers."
  (addressbar-eww--load-persistent-history)
  (addressbar-eww--load-bookmark)
  (when addressbar-eww--startup   ;; should be run only once
    (dolist (buf (buffer-list))
      (addressbar-eww--load-buffer-history buf))
    (setq addressbar-eww--startup nil))
  )

(defun addressbar-eww-list-candidates ()
  "Enumerate history, bookmarks, and browsing buffers."
  (addressbar-eww--update-entries)
  (hash-table-keys addressbar-eww--entries)
  )

(defun addressbar-eww-delete-entry (entry)
  "Delete selected url from history candidates. \
If bookmarked, also delete it."
  (when (eq (addressbar-eww--get-type entry) :bookmark)
    (let (bm)
      (dolist (bookmark eww-bookmarks)
        (if (not (equal entry (plist-get bookmark :url)))
            (push bookmark bm)
          ))
      (setq eww-bookmarks bm)
      (eww-write-bookmarks)
      ))
  (remhash entry addressbar-eww--entries)
  (addressbar-eww--save-persistent-history)
  )

;; mimics eww's function
(defun addressbar-eww-add-bookmark (entry)
  "Add selected url to eww's bookmark."
  (interactive)
  (eww-read-bookmarks)
  (dolist (bookmark eww-bookmarks)
    (when (equal entry (plist-get bookmark :url))
      (user-error "Already bookmarked")))
  (when (y-or-n-p "Bookmark this page?")
    (let ((title (replace-regexp-in-string "[\n\t\r]" " "
                                           (addressbar-eww--get-title entry))))
      (setq title (replace-regexp-in-string "\\` +\\| +\\'" "" title))
      (push (list :url entry
                  :title title
                  :time (current-time-string))
            eww-bookmarks)
      (message "Bookmarked %s (%s)" entry title)
      )
    (eww-write-bookmarks)
    ))

;;;###autoload
(defun addressbar-eww ()
  "Handy addressbar function for eww browser."
  (interactive)
  (eww (completing-read "EWW: " (sort (addressbar-eww-list-candidates) 'addressbar-eww--have-newer-timestamp))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(when (locate-library "ivy")
  (require 'ivy)

  (defun counsel-eww-browse (entry)
    "Open selected url, or switch buffer if it is already opened."
    (let ((buf (addressbar-eww--buffer-existp entry)))
      (if buf
          (switch-to-buffer buf)
        (eww entry))))

  (ivy-set-actions 'counsel-eww
                   '(("d" addressbar-eww-delete-entry "Delete URL")
                     ("o" brouse-url-generic "Open URL in generic-browser")
                     ("b" addressbar-eww-add-bookmark "Add URL to eww bookmark")
                     ("c" addressbar-eww-copy-link "Copy URL")
                     ))

;;;###autoload
  (defun counsel-eww ()
    "Onestop eww manager."
    (interactive)
    (ivy-read "EWW: " (addressbar-eww-list-candidates)
              :action 'counsel-eww-browse
              :history 'counsel-eww-history
              :sort t
              ))

  (add-to-list 'ivy-sort-functions-alist
             '(counsel-eww . addressbar-eww--have-newer-timestamp))

  (defun counsel-eww-display-type (entry)
    (pcase (addressbar-eww--get-type entry)
      (:bookmark "b")
      (:history "h")
      ))

  (defun counsel-eww-display-icon (entry)
      (all-the-icons-xxxx "XXXX" :height .9))

  (defun counsel-eww-display-time (entry)
    (format-time-string "%Y-%m-%d %H:%M" (seconds-to-time (addressbar-eww--get-time entry)))
    )

  (defun counsel-eww-display-url (entry)
    )

  (setq ivy-rich-display-transformers-list
        (plist-put ivy-rich-display-transformers-list
                   'counsel-eww
                   '(:columns
                     ((counsel-eww-display-type (:face success :width 1))
                      (counsel-eww-display-time (:width 16))
                      (ivy-rich-candidate (:width 0.7))
                      (addressbar-eww--get-title (:width 0.1 :face font-lock-doc-face))
                      ))
                   ))
  (if ivy-rich-mode (ivy-rich-reload))

  ;; ignore eww buffers from buffer list
  ;; company-dabbrev-ignore-buffers
  )

(provide 'addressbar-eww)
