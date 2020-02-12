(require 'eww)
(require 'subr-x)
(require 'cl-lib)
(require 'thingatpt)

;; custom variables

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

(defcustom addressbar-cleanup-threshold 10000
  "Histories are automatically pruned when exceeding this value, 0 means histories won't be housekept."
  )

(defcustom addressbar-copy-org-link nil
  ""
  )

(defcustom addressbar-display-url-max-length nil
  ""
  )

(defcustom addressbar-open-keywords '(
                                 ("g" . "https://google.com/search/")
                                 ("ej" . "https://ejje.weblio.jp/content/")
                                 )
  "You can define specific search command using this associated list."
  )

;; internal variables
(defvar addressbar--entries (make-hash-table :test #'equal)
  "Variable which holds addressbar histories."
  )

(defvar addressbar--debug nil)

(defvar addressbar--startup t)

;; internal functions

(defun addressbar--log (msg param)
  (if addressbar--debug
      (message "DEBUG: %s %s" msg param)
    ))

(defun addressbar--cleanup ()
  "delete oldest entries from candidates"
  (if (< addressbar-cleanup-threshold (hash-table-count addressbar--entries))
      (dolist (entry (cl-subseq (sort (hash-table-keys addressbar--entries) 'addressbar--have-newer-timestamp)
                                (round (* addressbar-cleanup-threshold .9))))
        (unless (eq (addressbar--get-type entry) :bookmark)
          (addressbar--log "cleanup:" entry)
          (remhash entry addressbar--entries)))))

(defun addressbar--have-newer-timestamp (x y)
  "compare entry's timestamp."
  (if (< (addressbar--get-time x)
         (addressbar--get-time y))
      nil t))

(defun addressbar--save-persistent-history ()
  "save current addressbar history into disk"
  (addressbar--cleanup)
  (with-temp-file (expand-file-name "addressbar-history" addressbar-persistent-history-directory)
    (insert ";; Auto-generated file; don't edit\n")
    (insert (prin1-to-string addressbar--entries))))

(defun addressbar--load-persistent-history ()
  "reload saved history"
  (let ((file (expand-file-name "addressbar-history" addressbar-persistent-history-directory)))
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
      (puthash url (list (if (addressbar--get-type url) :bookmark type)
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

(defun addressbar--buffer-existp (entry)
  "Return buffer which renders specified url, if exists."
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (if (derived-mode-p 'eww-mode)
          (if (string-equal (plist-get eww-data :url) entry)
              (return buf)))
      (if (derived-mode-p 'w3m-mode)
          (if (string-equal w3m-current-url entry)
              (return buf)))
      )))

;; w3m related functions
(when (locate-library "w3m")
  (require 'w3m)

  (defun addressbar--w3m-add-entry (type url title)
    (let (plist)
      (setq plist (plist-put plist :url url))
      (setq plist (plist-put plist :title title))
      (addressbar--add-entry type plist)))

  (defun addressbar--w3m-load-buffer-history (buf)
    (with-current-buffer buf
      (when (derived-mode-p 'w3m-mode)
        (dolist (his w3m-history-flat)
          (let ((url (car his))
                (title (cdadr his))
                plist)
            (unless (string-match "^about" url)
              (addressbar--w3m-add-entry :history url title)
              ))))))

  (defun addressbar--w3m-add-current-history (dummy)
    "Add or update addressbar candidate with current browsing page."
    (addressbar--w3m-add-entry :history w3m-current-url w3m-current-title)
    )
  (add-hook 'w3m-display-hook 'addressbar--w3m-add-current-history)
  )

(defun addressbar--update-entries ()
  "Collect entries of history hash. \
Once in startup, it also search history of eww buffers."
  (addressbar--load-persistent-history)
  (addressbar--eww-load-bookmark)
  (when addressbar--startup   ;; should be run only once
    (dolist (buf (buffer-list))
      (if (featurep 'w3m) (addressbar--w3m-load-buffer-history buf))
      (addressbar--eww-load-buffer-history buf)
      )
    (setq addressbar--startup nil))
  )

(defun addressbar-list-candidates ()
  "Enumerate history, bookmarks, and browsing buffers."
  (addressbar--update-entries)
  (hash-table-keys addressbar--entries)
  )

;; functions for entry manipulation

(defun addressbar-copy-link (entry)
  ""
  (let ((title (addressbar--get-title entry)))
    (if addressbar-copy-org-link
        (org-link-store-props
         :type "https"
         :link entry
         :url entry
         :description title)
      (kill-new entry)
      )))

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
  (addressbar--save-persistent-history))

(defun addressbar--thing-at-point ()
  (if current-prefix-arg
      (or (thing-at-point 'url t)
          (thing-at-point 'filename t)
          (thing-at-point 'symbol t)
          (thing-at-point 'word t)
          )))

(defun addressbar-open (select)
  "Open selected url, or switch buffer if it is already opened."
  (let ((buf (addressbar--buffer-existp select)))
    (if buf
        (switch-to-buffer buf)
      (let* ((input (split-string select))
             (keyword (assoc (car input) addressbar-open-keywords))
             )
        (if keyword
            (browse-url (concat (cdr keyword) (cadr input)))
          (browse-url select))))))

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
      (message "Bookmarked %s (%s)" entry title))
    (eww-write-bookmarks)))

;; functions for display data

(defun addressbar--display-url (entry)
  (let ((max (or addressbar-display-url-max-length
                 (round (* (frame-width) .7))))
        (len (length entry)))
    (substring entry 0 (if (> len max)
                           max len))))

(defun addressbar--display-type (entry)
  (pcase (addressbar--get-type entry)
    (:bookmark "b")
    (:history "h")
    ))
(defun addressbar--display-icon (entry)
  (all-the-icons-xxxx "XXXX" :height .9))

(defun addressbar--display-time (entry)
  (format-time-string "%Y-%m-%d %H:%M" (seconds-to-time (addressbar--get-time entry))))

;; functions for interactive interface

;;;###autoload
(defun addressbar ()
  "Handy addressbar function for emacs browser."
  (interactive)
  (addressbar-open (completing-read "Browse: "
                                    (sort (addressbar-list-candidates) 'addressbar--have-newer-timestamp)
                                    nil nil
                                    (addressbar--thing-at-point)
                                    )))

;;;###autoload
(defun ido-addressbar ()
    "ido wrapper of `addressbar'"
    (interactive)
    (let ((urls (mapcar (lambda (f)
                           (cons (addressbar--display-url f) f))
                         (sort (addressbar-list-candidates) 'addressbar--have-newer-timestamp))))
      (let ((select (ido-completing-read "Browse: " (mapcar #'car urls) nil nil (addressbar--thing-at-point))))
        (addressbar-open (assoc-default select urls)))))

(when (locate-library "ivy")
  (require 'ivy)

  (ivy-set-actions 'counsel-addressbar
                   '(("d" addressbar-delete-entry "Delete this URL")
                     ("o" brouse-url-generic "Open URL in generic-browser")
                     ("b" addressbar-add-bookmark "Add URL to eww bookmark")
                     ("c" addressbar-copy-link "Copy URL")
                     ))

;;;###autoload
  (defun counsel-addressbar ()
    "Onestop browsing manager."
    (interactive)
    (let ((thing (addressbar--thing-at-point)))
      (ivy-read "Browse: " (addressbar-list-candidates)
                :action 'addressbar-open
                :history 'counsel-addressbar-history
                :initial-input thing
                :sort t
                )))

  (add-to-list 'ivy-sort-functions-alist
             '(counsel-addressbar . addressbar--have-newer-timestamp))

  (setq ivy-rich-display-transformers-list
        (plist-put ivy-rich-display-transformers-list
                   'counsel-addressbar
                   '(:columns
                     ((addressbar--display-type (:face success :width 1))
                      (addressbar--display-time (:width 16))
                      (addressbar--display-url (:width 0.7))
                      (addressbar--get-title (:width 0.1 :face font-lock-doc-face))
                      ))
                   ))
  (if ivy-rich-mode (ivy-rich-reload))
  )

(provide 'addressbar)
