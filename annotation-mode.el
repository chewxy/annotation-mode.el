(require 'json)

;; utility functions

(defun print-region-info ()
  "prints the region info (USEFUL FOR DEBUGGING)"
  (interactive)
  (message "Region: %s %s"
	   (car (get-region-box))
	   (car (cdr (get-region-box)))))

(defun get-region-box ()
  "gets the region box"
  (if (use-region-p)
      (list (to-lc (region-beginning))
	    (to-lc (region-end)))
    nil))

(defun col-number-at-pos (pos)
  "like line-number-at-pos, but for cols (why is this not built in??)"
  (save-excursion
    (goto-char pos)
    (current-column)))

(defun to-lc (pos)
  "returns (line, column) pair given a pos. Line number starts at 0 because we're sane people"
  (list (1- (line-number-at-pos pos))
	(col-number-at-pos pos)))

;; user options

(defvar annotation-mode nil "if non nil, annotate turns on the annotation mode")

(make-variable-buffer-local 'annotate)

(put 'annotate 'variable-interactive "XTurn on annotation mode? (t, nil, or 'query): ")

(defvar annotation-file nil "if non-nil, the annotations will be written into the given file")

(make-variable-buffer-local 'annotation-file)

(put 'annotation-file 'variable-interactive "FSet annotation file: ")

(defvar annotation-list nil "if non-nil the annotations list will limit what annotations are available")

(put 'annotation-list 'annotation-list "XSet annotation list: ")

;; Cmd

(defun annotation-model (&optional arg)
  "Toggle `annotation' minor mode in this buffer."
  (interactive "P")
  (setq annotation-mode (if (null arg)
		     (not annotation-mode)
		   (> (prefix-numeric-value arg) 0)))
  (advice-add 'mouse-set-region :after #'register-mouse-select))

(defun turn-on-annotation-mode ()
  "Turn on `annotation' mode in this buffer.
This sets `annotation' to t."
  (interactive)
  (annotation-mode 1))

(defun turn-off-annotation-mode ()
  "Turn off `annotation' mode in this buffer.
This sets `annotation' to nil."
  (interactive)
  (annotation-mode -1))

(defun set-annotation-file (file)
  "sets the annotation file to use."
  (interactive "FSet the annotation file (JSON): ")
  (setq annotation-file file)
  (if (not (file-exists-p file)) ;; create file if not found
      (write-region "{}" nil file)))

(defun set-annotation-dir (dir)
  "sets the annotation file to use by providing the directory to save the file in. The filename will be the same as the current buffer's file name, with a .json appended to it."
  (interactive "DSet where to save the annotation file: ")
  (let* ((bn (file-name-nondirectory (buffer-file-name (window-buffer (minibuffer-selected-window)))))
	 (file (concat dir bn ".json")))
    (setq annotation-file file)
    (if (not (file-exists-p file))
	(write-region "{}" nil file))
    (message "Annotations will be saved to `%s'" file)))

(defun unset-annotation-file ()
    "unsets the annotation file to use."
    (interactive)
    (setq annotation-file nil))

(defun set-valid-annotations (valid)
  "sets the list of valid annotations"
  (interactive "xProvide a list of valid annotations: ")
  (if (listp valid)
      (setq annotation-list valid)))

(defun set-valid-annotations-file (file)
  "sets the valid annotations to be used."
  (interactive "FList of valid annotations (JSON): ")
  (let* ((json-array-type 'list)
	 (existing (json-read-file file)))
       (if (not (null existing))
	   (setq annotation-list existing)
	 (setq annotation-list nil))))

(defun unset-valid-annotations ()
  "sets the annotation-list variable to nil"
  (interactive)
  (setq annotation-list nil))

(defun annotate ()
  "annotates a region."
  (interactive)
  (if (not (null annotation-mode))
      (make-annotation)))


;; Internal functions


(defun what-class (x)
  "sets the classification of the region - returns a alist
     ((:class x)
      (:region_start begin)
      (:region_end end))"
  (interactive
   (if annotation-list
       (let ((completion-ignore-case t))
	 (list (completing-read "What class should this region be? " annotation-list nil t)))
   "sWhat class should this region be? "))
  (let ((reg (get-region-box)))
    (list (cons "class" (eval x))
	  (cons "region_start" (vconcat [] (car reg)))
	  (cons "region_end"  (vconcat [] (car (cdr reg)))))))

(defun make-annotation ()
  "make-annotation  checks that there is a given region. Then adds the annotaion to a JSON file if its provided. Otherwise it wiill be displayed as a message."
  (let ((reg (get-region-box)))
     (if (not (null reg))
      (if (not (null annotation-file))
	  (add-annotation-json-obj (call-interactively 'what-class))
	(message "%s" (json-encode (call-interactively 'what-class)))))))


(defun register-mouse-select (&rest args)  (annotate))

(defun check-annotation-mode ()
  (interactive)
  (message "annotate %s"
	   (not (null annotation-mode))))


(defun add-annotation-json-obj (obj)
  "adds a annotation object to the existing json file."
  ;; TODO: maybe consider using a local variable so we don't have to read the file all the time?
  ;; (setq-local existing (json-read-file annotation-file))
  ;; (if (null existing))
  ;;     (setq-local existing obj)
  ;;     (setq-local existing (list existing obj)))
  (let* ((json-object-type 'alist)
	 (json-array-type 'vector)
	 (json-key-type 'string)
	 (existing (json-read-file annotation-file)))
    (if (null existing)
	(write-region (json-encode obj)
		      nil
		      annotation-file)
      (if (vectorp existing)
	    (write-region (json-encode (vconcat existing (list obj)))
			  nil
			  annotation-file)
	  (write-region (json-encode-list (list existing obj))
			nil
			annotation-file)))))

(define-minor-mode annotation-mode
  "Create annotations in emacs. Duh. Use set-annotation-file to set a file to write the annotations to. Use set-valid-annotations or set-valid-annotations-file to limit the annotation (this prevents errors)."
  :lighter " annotation"
  (advice-add 'mouse-set-region :after #'register-mouse-select)
  (read-only-mode))

(provide 'annotation-mode)
