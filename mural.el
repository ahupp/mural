;;
;; mural does fast, fuzzy tag searching that refines results as you
;; type.
;;
;; To use, put this in your .emacs:
;;
;;   (require 'mural)
;;   ;; not strictly necessary, but avoids slowness on first use
;;   (mural-get-server)
;;   (global-set-key (kbd "C-o") 'mural-find-tag)
;;
;; If it is not already set make sure tags-file-name points at your tags file.
;;
;; To debug launch:
;; (call-process mural-server-path nil t nil tags-file-name)

(require 'ido)

(defvar mural-server-path "mural_server"
  "Path to the mural_server executable")

(defvar mural-server-echo nil
  "echo server responses to *muralserver* buffer")
(defvar mural-server-process nil)
(defvar mural-server-output "")
(defvar ido-dynamic-match-fn nil)
(defvar mural-last-query-result nil)

(defun mural-get-server ()
  (if (or (not mural-server-process)
          (not (eq (process-status mural-server-process) 'run)))
      (setq mural-server-process (mural-start-server)))
  mural-server-process)

(defun mural-start-server ()
  (if (not tags-file-name)
      (error "tags-file-name not set"))
  (message "Starting mural server")
  (let ((proc (start-process
               "mural" "*muralserver*"
               mural-server-path tags-file-name)))
    (set-process-filter
     proc
     (lambda (process output)
       (with-current-buffer (process-buffer process)
         (save-excursion
           (if mural-server-echo
               (progn
                 (goto-char (process-mark process))
                 (insert output)
                 (set-marker (process-mark process) (point))))
           (setq mural-server-output (concat mural-server-output output))))))
     proc))

(defun mural-query (query)
  (if (equal "" query)
      '()
    (progn
      (setq mural-server-output "")
      (process-send-string (process-name (mural-get-server))
                           (concat query "\n"))

      ;; Wait up to 1/2 second
      (loop for i from 1 to 10
            until (mural-response-complete mural-server-output)
            do (accept-process-output mural-server-process .05))
      (if (mural-response-complete mural-server-output)
          (mural-parse-response mural-server-output)
        (progn
          (message "mural-server timeout")
          '())
        ))))

(defun mural-query-save-tag (query)
  (let ((qresult (mural-query query)))
    (setq mural-last-query-result qresult)
    (mapcar (lambda (x) (elt x 0)) qresult)))

;;(mural-parse-response "MATCH foo bar 1\nMATCH hi bye 2\nDONE hi hi")
;; returns '((tag file row) ...)
(defun mural-parse-response (results)
  (delq nil
        (mapcar
         (lambda (x)
           ;; spaces in the filename?  tough.
           (let ((row (split-string x)))
             (if (equal (elt row 0) "MATCH")
                 ;; MATCH tag filename row
                 (list (elt row 1) (elt row 2) (string-to-number (elt row 3)))
               ;; hopefully DONE
               nil)))
         (split-string results "\n"))))

(defun mural-response-complete (result)
  (integerp (string-match "^DONE" result)))

(defun mural-read-tag ()
  (setq ido-dynamic-match-fn 'mural-query-save-tag)
  (unwind-protect ;; i have no idea what's going on here
      (ido-completing-read "tag: " '("nope"))
    (setq ido-dynamic-match-fn nil)))

(defun mural-tag-filename (tag)
  (elt tag 1))
(defun mural-tag-row (tag)
  (elt tag 2))


(defun mural-open-dwim ()
  (interactive)
  (let* ((tag (mural-read-tag))
        (taginfo (assoc tag mural-last-query-result)))
    (progn
      (find-file (mural-abspath-for-tag taginfo))
      (goto-line (mural-tag-row taginfo))
      (setq mural-last-query-result nil)
      )))

(defun mural-abspath-for-tag (taginfo)
  (expand-file-name
   (mural-tag-filename taginfo)
   ;; need to make this conditional on the tag file to support multiple repos
   (file-name-directory (directory-file-name tags-file-name))))

;; (mural-abspath-for-tag '("hi" "flib/core/preparable/Preparer.php" 1))

(defvar ido-dynamic-last-query nil)

(defadvice ido-set-matches (before ido-dynamic-match ())
  (if (and ido-dynamic-match-fn
           ;; only reset ido-cur-list if the query has changed.  This
           ;; prevents resetting the results during rotation (C-s)
           (not (equal ido-dynamic-last-query ido-text)))
      (progn
        (setq ido-dynamic-last-query ido-text)
        (setq ido-cur-list (funcall ido-dynamic-match-fn ido-text)))))

(ad-activate 'ido-set-matches)

(provide 'mural)

