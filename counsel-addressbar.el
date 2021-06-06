(require 'ivy)
(require 'addressbar)

;;;###autoload
(defun counsel-addressbar ()
  "`counsel' wrapper of `addressbar'."
  (interactive)
  (let ((thing (addressbar--thing-at-point)))
    (ivy-read "Browse: " (addressbar-list-candidates)
              :action 'addressbar-open
              :history 'counsel-addressbar-history
              :initial-input thing
              :sort t)))

(ivy-set-actions 'counsel-addressbar
                 '(("d" addressbar-delete-entry "Delete this URL")
                   ("e" brouse-url-generic "Open URL in generic-browser")
                   ("b" addressbar-add-bookmark "Add URL to eww bookmark")
                   ("c" addressbar-copy-link-org "Copy URL as org style")))

(add-to-list 'ivy-sort-functions-alist
             '(counsel-addressbar . addressbar--have-newer-timestamp))

(when (locate-library "ivy-rich")
  (with-eval-after-load "ivy-rich"
    (defun addressbar--ivy-rich-setup ()
      (setq ivy-rich-display-transformers-list
            (plist-put ivy-rich-display-transformers-list
                       'counsel-addressbar
                       `(:columns
                         ((addressbar--display-type (:face success :width 2))
                          (addressbar--display-time (:width ,(length (format-time-string addressbar-time-format (current-time)))))
                          (addressbar--display-url (:width ,(addressbar--column-width :url)))
                          (addressbar--get-title (:width ,(addressbar--column-width :title) :face font-lock-doc-face))))))
      (if ivy-rich-mode (ivy-rich-reload)))
    (addressbar--ivy-rich-setup)
    (if ivy-posframe-mode
        (add-hook 'window-size-change-functions 'addressbar--ivy-rich-setup))
    ))

(provide 'counsel-addressbar)
