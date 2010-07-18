(defvar ii-temp-file     "/tmp/iie.tmp"
  "Temporary file to save messages before catting them into ii input fifos")
(defvar ii-irc-directory "~/irc/"
  "Directory to look for ii files in. end with slash.")
(defvar ii-prompt-marker nil
  "Marker that keeps track of where the prompt starts")
(defvar ii-prompt-text "ii> "
  "Prompt text used")
(defvar ii-inotify-process nil
  "The inotify process")
(defvar ii-channel-sizes (make-hash-table :test 'equal)
  "Keeps track of channel-file sizes for reading latest input only")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; database
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ii-filesize (file)
  (nth 7 (file-attributes file)))

(defun ii-get-channels ()
  (split-string (shell-command-to-string (concat "find " ii-irc-directory " -name out")) "\n"))

(defun ii-get-channel-sizes ()
  (dolist (outfile (ii-get-channels))
    (puthash outfile (ii-filesize outfile) ii-channel-sizes)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; inotify
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ii-setup-maybe ()
  "If not already running, start the process and setup buffer sizes."
  (unless (and ii-inotify-process
	       (= (process-exit-status ii-inotify-process) 0))
    (ii-get-channel-sizes)
    (setf ii-inotify-process
	  (start-process "ii-inotify" nil "inotifywait" "-mr" ii-irc-directory))
    (set-process-filter ii-inotify-process 'ii-handle-inotify)))

(defun ii-handle-inotify (process output)
  "Split inotify output into lines and dispatch on relevant changes"
  (dolist (line (split-string output "\n"))
    (when (string-match "\\(.*\\) CLOSE_WRITE,CLOSE out" line)
      (ii-handle-file-update (concat (match-string 1 line) "out")))))

(defun ii-handle-file-update (file)
  (let ((delta      (get-file-delta file))
  	(buffer (get-file-buffer file)))
    (when (and delta buffer)
      ;; Affected file is being changed and visited
      (with-current-buffer buffer
	(let ((inhibit-read-only t)
	      (marker-from-end (- (point-max) (point))))
	  (goto-char ii-prompt-marker)
	  (insert-before-markers (propertize delta 'read-only t))
	  (goto-char (- (point-max) marker-from-end)))))))

(defun get-file-delta (file)
  (let ((old-size (gethash file ii-channel-sizes 0))
  	(new-size (ii-filesize file)))
    ;; update old value
    (unless (= old-size new-size)
      (setf (gethash file ii-channel-sizes) new-size)
      (with-temp-buffer
	(save-excursion
	  (insert-file-contents file nil old-size new-size)
	  (buffer-string))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-derived-mode ii-mode fundamental-mode "ii" (ii-mode-init))

(defvar ii-mode-map nil)
(setq ii-mode-map (let ((map (make-sparse-keymap)))
		    (define-key map [remap save-buffer] (lambda () (interactive) (message "nop")))
		    (define-key map [remap move-beginning-of-line] 'ii-beginning-of-line)
		    (define-key map "\C-c\C-s" 'ii-send-message)
		    map))

(defun ii-mode-init ()
  (use-local-map ii-mode-map)
  ;; disable autosave and "modified; kill anyway?"
  (auto-save-mode nil)

  ;; rename buffer
  (let ((name (first (last (split-string (file-name-directory (buffer-file-name)) "/") 2))))
    (rename-buffer (generate-new-buffer-name name)))

  ;; local variables.  
  (set (make-local-variable 'ii-prompt-marker) (make-marker))

  ;; insert prompt and make log readonly.
  (goto-char (point-max))
  (set-marker ii-prompt-marker (point))
  (insert ii-prompt-text)
  ;; make it all readonly
  (let ((inhibit-read-only t))
    (put-text-property (point-min) (1+ (point-min)) 'front-sticky t)
    (put-text-property (point-min) (point-max) 'read-only t)
    (put-text-property (1- (point-max)) (point-max) 'rear-nonsticky t))  
  (ii-setup-maybe)
  (goto-char (point-max)))
	     
;; movement

(defun ii-beginning-of-line ()
  (interactive)
  (if (> (point) ii-prompt-marker)
      (goto-char (+ ii-prompt-marker (length ii-prompt-text)))
    (move-beginning-of-line nil)))

;; sending messages

(defun ii-send-message ()
  (interactive)
  (let ((fifo-in (concat (file-name-directory (buffer-file-name)) "in")))
    (unless (file-exists-p fifo-in)
      (error "Invalid channel directory"))
    ;; semi-hack: catting tmpfile asynchronously to fifo to prevent lockups if
    ;; nothing is reading in the other end
    (write-region (concat (ii-clear-and-return-prompt) "\n")
		  nil ii-temp-file nil 
		  ;; If VISIT is neither t nor nil nor a string,
		  ;; that means do not display the "Wrote file" message.
		  0)
    (start-process-shell-command 
     "ii-sendmessage" nil
     (concat "cat " ii-temp-file " > \"" fifo-in "\""))))

(defun ii-clear-and-return-prompt ()
  (let* ((start-pos (+ ii-prompt-marker (length ii-prompt-text)))
	 (text (buffer-substring start-pos (point-max))))
    (delete-region start-pos (point-max))
    text))

;; overview mode TBD

(define-derived-mode ii-overview-mode fundamental-mode "ii-overview"
   (use-local-map ii-overview-mode-map))

(let ((map (make-sparse-keymap)))
  (define-key map "RET" 'ii-overview-open-buffer)
  (setq ii-overview-mode-map map))

(defun ii-overview-follow-link ()
  (interactive))

(defun ii-overview ()
  (interactive)
  (switch-to-buffer (get-buffer-create "*ii-overview*"))
  (ii-overview-mode)
  (save-excursion 
    (let ((inhibit-read-only t))
      (dolist (channel (ii-get-channels))
	(insert
	 (propertize (concat channel "\n")
		     'read-only t
		     'front-sticky t
		     'channel-path channel))))))

;; provide

(provide 'ii-mode)