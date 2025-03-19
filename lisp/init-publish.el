;;; init-publish.el --- Publishing configuration for ox-hugo -*- lexical-binding: t; -*-

;; Define helper functions (taken and adapted from Justin Abrahms’ config):

(defun my/flatten-list (list-of-lists)
  "Flatten a LIST-OF-LISTS into a single list."
  (apply 'append list-of-lists))

(defun my/string-lists-intersect-p (list1 list2)
  "Return t if LIST1 and LIST2 share any element, nil otherwise."
  (not (null (cl-intersection list1 list2 :test 'string=))))

(defun my/org-get-filetags (e)
  "Extract file tags from an Org element E (look for the FILETAGS keyword)."
  (let ((type (org-element-type e))
        (key (org-element-property :key e)))
    (if (and (eq type 'keyword) (equal key "FILETAGS"))
        (split-string (org-element-property :value e) ":"))))

;; org-export-exclude-tags
(setq org-export-exclude-tags '("noexport" "daily"))

;; Define the custom publishing function
(defun my/ox-hugo-org-publish (plist filename pub-dir)
  "Publish the Org file FILENAME using ox-hugo.
PLIST is the export property list, and PUB-DIR is the publishing directory.
Files whose names match \"/daily/\" are skipped, and files with excluded tags (if
`org-export-exclude-tags' is set) will not be exported."
  (message "Exporting %s" filename)
  (if (not (string-match-p (regexp-quote "/daily/") filename))
      (with-current-buffer (find-file-noselect filename)
        (let ((tags (my/flatten-list (org-element-map (org-element-parse-buffer) 'keyword #'my/org-get-filetags))))
          (if (not (my/string-lists-intersect-p tags org-export-exclude-tags))
              (org-hugo--export-file-to-md filename)
            (message "Skipped export. Exclude tags found: %s" tags)))))
  nil)

;; Define the publishing project.
;; Adjust the base directory as needed: for example, if your org-roam files you wish to publish are in "~/org/org-roam/",
;; change the path accordingly.
(setq org-publish-project-alist
      '(("quartz"
         :base-directory "~/org/org-roam/"         ; or wherever your main org-roam files live
         :publishing-directory "~/quartz/"           ; destination for exported files
         :select-tags '()                            ; adjust if needed
         :recursive t
         :publishing-function my/ox-hugo-org-publish
         :author "Boyang Yan"
         :email "yanboyang713@gmail.com")))

(provide 'init-publish)
;;; init-publish.el ends here
