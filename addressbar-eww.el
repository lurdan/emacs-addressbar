(require 'eww)
(require 'subr-x)

(defvar addressbar-eww--entries (make-hash-table :test #'equal))
(defvar addressbar-eww-persistent-history-file)
(defvar addressbar-eww-cleanup-threshold 0)

(defun addressbar-eww--save-persistent-history ()
  "TODO: save current addressbar history into disk"
  (if (< addressbar-eww-cleanup-threshold (hash-table-count addressbar-eww--entries))
      (addressbar-eww--cleanup))
  ;; addressbar-eww-persistent-history-file
  ;; (prin1-to-string addressbar-eww--entries))
  )

(defun addressbar-eww--load-persistent-history ()
  "TODO: reload saved history"
  ;; addressbar-eww-persistent-history-file
  ;; (read (buffer-string))
  )

(defun addressbar-eww--have-newer-timestamp ()
  "TODO: compare entry's timestamp. will required for cleanup and sort"
  )

(defun addressbar-eww--cleanup ()
  "TODO: delete oldest entries from candidates"
  )

(defun addressbar-eww-sort ()
  "TODO: sort candidates by its :time"
  )

(defun addressbar-eww--get-entry-metadata (label entry)
  "Getter of addressbar candidates."
  (let ((meta (gethash entry addressbar-eww--entries)))
    (cond
     ((eq label :type) (car meta))
     ((eq label :title) (cadr meta))
     ((eq label :time) (cadr meta))
     )))

(defun addressbar-eww--add-entry (type plist)
  "Setter of addressbar candidates."
  (let ((url (plist-get plist :url))
        (title (plist-get plist :title))
        (time (plist-get plist :time)))
    (puthash url (list type
                       title
                       (time-to-seconds (if time
                                            (decode-time (parse-time-string time))
                                          (current-time))))
             addressbar-eww--entries)))

(defun addressbar-eww--load-bookmark ()
  "Convert bookmark of eww into addressbar candidates."
  (eww-read-bookmarks)
  (dolist (bookmark eww-bookmarks)
    (addressbar-eww--add-entry :bookmark bookmark)
    ))

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
          (if (eq (plist-get eww-data :url) entry)
              buf)))))

;;(clrhash addressbar-eww--entries)
(defun addressbar-eww--update-entries ()
  "Collect entries of history hash. \
Once in startup, it also search history of eww buffers."
  (and (eq (hash-table-count addressbar-eww--entries) 0)
       (dolist (buf (buffer-list))
         (addressbar-eww--load-buffer-history buf))
       )
  (addressbar-eww--load-persistent-history)
  (addressbar-eww--load-bookmark)
  )

(defun addressbar-eww-list-candidates ()
  "Enumerate history, bookmarks, and browsing buffers."
  (addressbar-eww--update-entries)
  (hash-table-keys addressbar-eww--entries)
  )

(defun addressbar-eww-delete-entry (entry)
  "Delete selected url from history candidates. \
If bookmarked, also delete it."
  (when (eq (addressbar-eww--get-entry-metadata :type entry) :bookmark)
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
                                           (addressbar-eww--get-entry-metadata entry :title))))
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
  (eww (completing-read "EWW: " (addressbar-eww-list-candidates))))

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
              :sort 'sort
              ))

  ;; (eval-after-load 'ivy-rich
  ;;   (setq ivy-rich-display-transformers-list (plist-put ivy-rich-display-transformers-list
  ;;                                                       'counsel-eww
  ;;                                                       '(:columns ((counsel-eww-transformer (:width 40))
  ;;                                                                   (counsel-eww-entry-type)
  ;;                                                                   (counsel-eww-entry-title (:face font-lock-dock-face))
  ;;                                                                   (counsel-eww-entry-timestamp)
  ;;                                                                   ))))
  ;;         ))

  ;; ignore eww buffers from buffer list
  ;; (add-to-list 'ivy-ignore-buffers "\\ \*eww")
  ;; company-dabbrev-ignore-buffers
  )

(provide 'addressbar-eww)
