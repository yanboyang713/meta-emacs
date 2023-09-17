;;; init-funcs.el --- record functions -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
;;;;;;;;;;;;;;;;;;;
;; New functions ;;
;;;;;;;;;;;;;;;;;;;
;(defun ws/verify-refile-target ()
;  "Exclude todo keywords with a done state"
;  (not (member (nth 2 (org-heading-components)) org-done-keywords)))

(defun getcal (url file)
  "Download ics file and add it to file"
  (let ((tmpfile (url-file-local-copy url)))
    (icalendar-import-file tmpfile file)
    (kill-buffer (car (last (split-string tmpfile "/"))))))

(defun getcals ()
  "Load a set of ICS calendars into Emacs diary files"
  (interactive)
  (mapcar #'(lambda (x)
	      (let ((file (concat diary-location (car x)))
		    (url (cdr x)))
		(message (concat "Loading " url " into " file))
		(find-file file)
		;; (flush-lines "^[& ]") ;; if you import ical as non marking
		(erase-buffer) ;; to avoid duplicating events
		(getcal url file)
		))
	  calendars))


(defun delete-visited-file (buffer-name)
  "Delete the file visited by the buffer named BUFFER-NAME."
  (interactive "bDelete file visited by buffer ")
  (let* ((buffer (get-buffer buffer-name))
	 (filename (buffer-file-name buffer)))
    (when buffer
      (when (and filename
		 (file-exists-p filename))
	(delete-file filename))
      (kill-buffer buffer))))

(defun kk/delete-frame-if-neccessary (&rest r)
  (cond
   ((= kk/delete-frame-after-capture 0) nil)
   ((> kk/delete-frame-after-capture 1)
    (setq kk/delete-frame-after-capture (- kk/delete-frame-after-capture 1)))
   (t
    (setq kk/delete-frame-after-capture 0)
    (delete-frame))))

;(defun ws/verify-refile-target ()
;  "Eclude todo keywords with a done state"
;  (not (member (nth 2 (org-heading-components)) org-done-keywords)))

(defun my/org-ref-open-pdf-at-point ()
  "Open the pdf for bibtex key under point if it exists."
  (interactive)
  (let* ((results (org-ref-get-bibtex-key-and-file))
	 (key (car results))
	 (pdf-file (funcall org-ref-get-pdf-filename-function key)))
    (if (file-exists-p pdf-file)
	(find-file pdf-file)
      (message "No PDF found for %s" key))))

(defun org-export-latex-no-toc (depth)
  (when depth
    (format "%% Org-mode is exporting headings to %s levels.\n"
	    depth)))

(defun org-roam-capture-pdf-active-region ()
  (let* ((pdf-buf-name (plist-get org-capture-plist :original-buffer))
	 (pdf-buf (get-buffer pdf-buf-name)))
    (if (buffer-live-p pdf-buf)
	(with-current-buffer pdf-buf
	  (car (pdf-view-active-region-text)))
      (user-error "Buffer %S not alive" pdf-buf-name))))

;; Customize org-roam-minibuffer
(setq org-roam-node-display-template (concat "${title:100} "
					     (propertize "${tags:20}" 'face 'org-tag)))

;; I shamelessly copy-pasted these from doom emacs, because they are super useful.
(defun +org-realign-table-maybe-h ()
  "Auto-align table under cursor."
  (when (and org-table-automatic-realign (org-at-table-p) org-table-may-need-update)
    (let ((pt (point))
          (inhibit-message t))
      (if org-table-may-need-update (org-table-align))
      (goto-char pt))))

(defun +org-enable-auto-reformat-tables-h ()
  "Realign tables & update formulas when exiting insert mode (`evil-mode').
Meant for `org-mode-hook'."
  (when (featurep 'evil)
    (add-hook 'evil-insert-state-exit-hook #'+org-realign-table-maybe-h nil t)
    (add-hook 'evil-replace-state-exit-hook #'+org-realign-table-maybe-h nil t)
    (advice-add #'evil-replace :after #'+org-realign-table-maybe-a)))

(defun +org-realign-table-maybe-a (&rest _)
  "Auto-align table under cursor and re-calculate formulas."
  (when (eq major-mode 'org-mode)
    (+org-realign-table-maybe-h)))

;; Everything below, so far, was shamelessly copied from doom emacs
(defun +org--insert-item (direction)
  (let ((context (org-element-lineage
                  (org-element-context)
                  '(table table-row headline inlinetask item plain-list)
                  t)))
    (pcase (org-element-type context)
      ;; Add a new list item (carrying over checkboxes if necessary)
      ((or `item `plain-list)
       (let ((orig-point (point)))
         ;; Position determines where org-insert-todo-heading and `org-insert-item'
         ;; insert the new list item.
         (if (eq direction 'above)
             (org-beginning-of-item)
           (end-of-line))
         (let* ((ctx-item? (eq 'item (org-element-type context)))
                (ctx-cb (org-element-property :contents-begin context))
                ;; Hack to handle edge case where the point is at the
                ;; beginning of the first item
                (beginning-of-list? (and (not ctx-item?)
                                         (= ctx-cb orig-point)))
                (item-context (if beginning-of-list?
                                  (org-element-context)
                                context))
                ;; Horrible hack to handle edge case where the
                ;; line of the bullet is empty
                (ictx-cb (org-element-property :contents-begin item-context))
                (empty? (and (eq direction 'below)
                             ;; in case contents-begin is nil, or contents-begin
                             ;; equals the position end of the line, the item is
                             ;; empty
                             (or (not ictx-cb)
                                 (= ictx-cb
                                    (1+ (point))))))
                (pre-insert-point (point)))
           ;; Insert dummy content, so that `org-insert-item'
           ;; inserts content below this item
           (when empty?
             (insert " "))
           (org-insert-item (org-element-property :checkbox context))
           ;; Remove dummy content
           (when empty?
             (delete-region pre-insert-point (1+ pre-insert-point))))))
      ;; Add a new table row
      ((or `table `table-row)
       (pcase direction
         ('below (save-excursion (org-table-insert-row t))
                 (org-table-next-row))
         ('above (save-excursion (org-shiftmetadown))
                 (+org/table-previous-row))))

      ;; Otherwise, add a new heading, carrying over any todo state, if
      ;; necessary.
      (_
       (let ((level (or (org-current-level) 1)))
         ;; I intentionally avoid `org-insert-heading' and the like because they
         ;; impose unpredictable whitespace rules depending on the cursor
         ;; position. It's simpler to express this command's responsibility at a
         ;; lower level than work around all the quirks in org's API.
         (pcase direction
           (`below
            (let (org-insert-heading-respect-content)
              (goto-char (line-end-position))
              (org-end-of-subtree)
              (insert "\n" (make-string level ?*) " ")))
           (`above
            (org-back-to-heading)
            (insert (make-string level ?*) " ")
            (save-excursion (insert "\n"))))
         (when-let* ((todo-keyword (org-element-property :todo-keyword context))
                     (todo-type    (org-element-property :todo-type context)))
           (org-todo
            (cond ((eq todo-type 'done)
                   ;; Doesn't make sense to create more "DONE" headings
                   (car (+org-get-todo-keywords-for todo-keyword)))
                  (todo-keyword)
                  ('todo)))))))

    (when (org-invisible-p)
      (org-show-hidden-entry))
    (when (and (bound-and-true-p evil-local-mode)
               (not (evil-emacs-state-p)))
      (evil-insert 1))))

(defun +org--insert-item-below (count)
  "Inserts a new heading, table cell or item below the current one."
  (interactive "p")
  (dotimes (_ count) (+org--insert-item 'below)))

(defun +org--insert-item-above (count)
  "Inserts a new heading, table cell or item above the current one."
  (interactive "p")
  (dotimes (_ count) (+org--insert-item 'above)))

(defun +flyspell-save-word ()
  (interactive)
  (let ((current-location (point))
         (word (flyspell-get-word)))
    (when (consp word)    
      (flyspell-do-correct 'save nil (car word) current-location (cadr word) (caddr word) current-location))))

(provide 'init-funcs)
;;; init-funcs.el ends here
