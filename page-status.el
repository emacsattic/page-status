;;; Time-stamp: <2005-01-18 12:05:56 john>
;;; <title>Handle page status markers, by my conventions</title>

(provide 'page-status)
(require 'find-page-file)

(defun find-page-property (page pattern &optional default-value)
  "Return the property of PAGE matched by first marking in PATTERN
or DEFAULT-VALUE which defaults to nil."
  (cond
   ((stringp pattern)
    (with-page-file
     page
     (goto-char (point-min))
     (message "Looking for \"%s\" in %s" pattern page)
     (let ((result (if (re-search-forward pattern
					  (point-max) t)
		       (buffer-substring (match-beginning 1) (match-end 1))
		     default-value)))
       (message "Result is %s" result)
       result)))
   ((listp pattern)
    (catch 'found
      (while pattern
	(let ((x (find-page-property page (car pattern) nil)))
	  (if x
	      (throw 'found x)
	    (setq pattern (cdr pattern)))))
      default-value))))

(defvar html-status-regexp
  "<!-- status: \\([^<]+\\)-->"
  "Pattern for status markers in my web pages.")

(defvar html-priority-regexp
  "<!-- priority: \\([^<]+\\)-->"
  "Pattern for priority markers in my web pages.")

(defvar html-started-regexp
  "<!-- started: \\([^<]+\\)-->"
  "Pattern for started markers in my web pages")

;;;###autoload
(defun html-move-status-markers-to-safety ()
  "Move my status etc markers to safety for re-tailing"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward html-status-regexp (point-max) t)
	(let ((status (match-string 1)))
	  (delete-region (match-beginning 0)
			 (match-end 0))
	  (goto-char (point-min))
	  (search-forward "<body>" (point-max) t)
	  (insert "\n<!-- status: " status " -->\n")))
    (goto-char (point-min))
    (if (re-search-forward html-started-regexp (point-max) t)
	(let ((started (match-string 1)))
	  (delete-region (match-beginning 0)
			 (match-end 0))
	  (goto-char (point-min))
	  (search-forward "<body>" (point-max) t)
	  (insert "\n<!-- started: " started " -->\n")))
    (goto-char (point-min))
    (if (re-search-forward html-priority-regexp (point-max) t)
	(let ((priority (match-string 1)))
	  (delete-region (match-beginning 0)
			 (match-end 0))
	  (goto-char (point-min))
	  (search-forward "<body>" (point-max) t)
	  (insert "\n<!-- priority: " priority: " -->\n")))))

;;;###autoload
(defun link-update-title (where)
  "Update the title of the link at or before point."
  (interactive "d")
  (save-excursion
    (let ((start nil) (end nil) (title nil))
      (goto-char where)
      (if (and (setq end (search-forward "</a>" (point-max) t))
	       (setq start (progn (goto-char (setq end (- end 4)))
				  (search-backward ">" (point-min) t)))
	       (progn (goto-char (1- (point))) t)
	       (setq title (find-page-title (browse-url-at-point))))
	  (progn
	    (delete-region (1+ start) end)
	    (search-forward "><" (point-max) t) (goto-char (1- (point)))
	    (insert title))))))

;;;###autoload
(defun find-page-title (url &optional default-title)
  "Return the title of URL, or DEFAULT-TITLE which defaults to nil"
  (interactive (browse-url-interactive-arg "Find title for URL: "))
  (let ((x
  (find-page-property
   url
   '("<title>\\([^<]+\\)</title>"
     "<!--#set var=\"title\" value=\"\\([^\"]+\\)\" -->")
   default-title)))
    (message "Found page title \"%s\" for page \"%s\"" x url)
    x))

;;;###autoload
(defun find-page-status (url &optional default-status)
  "Return the status of URL, or DEFAULT-STATUS which defaults to nil"
  (interactive (browse-url-interactive-arg "Find status for URL: "))
  (find-page-property url html-status-regexp default-status))

;;;###autoload
(defun find-page-revision-priority (url &optional default-priority)
  "Return the revision priority of URL, or DEFAULT-PRIORITY which defaults to nil"
  (interactive (browse-url-interactive-arg "Find priority for URL: "))
  (find-page-property url html-priority-regexp default-priority))

;;;###autoload
(defun find-page-started (url &optional default-started)
  "Return the started of URL, or DEFAULT-STARTED which defaults to nil"
  (interactive (browse-url-interactive-arg "Find started date for URL: "))
  (find-page-property url html-started-regexp default-started))

;;;###autoload
(defun find-page-content-length (url)
  "Return the real content length of URL (by my own conventions)."
  (interactive (browse-url-interactive-arg "Find content length for URL: "))
  (let ((content-length
	 (save-window-excursion
	   (if (find-page-file url)
	       (save-excursion
		 (let ((start (point-min)) (end (point-max)))
		   (goto-char (point-min))
		   (if (search-forward "<body>" (point-max) t)
		       (setq start (point)))
		   (goto-char (point-max))
		   (if (search-backward "<hr>" start t)
		       (setq end (point)))
		   (- end start)))
	     0))))
    (if (interactive-p)
	(message "Content length is %d" content-length))
    content-length))

;;; end of page-status.el
