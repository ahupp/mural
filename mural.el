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


(require 'ido)

(defvar mural-server-path "mural_server"
  "Path to the mural_server executable")

(defvar mural-server-process nil)
(defvar mural-server-output "")
(defvar ido-dynamic-match-fn nil)

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
       (setq mural-server-output (concat mural-server-output output))))
  proc))

(defun mural-query (query)
  (if (equal "" query)
      '()
    (progn
      (setq mural-server-output "")
      (process-send-string (process-name (mural-get-server))
                           (concat query "\n"))

      ;; Wait up to 2/10 of a second
      (loop for i from 1 to 20
            until (mural-response-complete mural-server-output)
            do (accept-process-output mural-server-process .01))
      (if (not (mural-response-complete mural-server-output))
          (progn
            (kill-process (mural-get-server))
            (setq mural-server-process nil
                  mural-server-output "")
            (error "failed to get response from server")))
      (mural-parse-response mural-server-output))))

(defun mural-parse-response (results)
  (mapcar (lambda (x) (substring x (length "MATCH ")))
          (delete "DONE" (split-string results "\n" t))))


(defun mural-response-complete (result)
  (and (> (length result) 3)
       (equal "DONE\n" (substring result -5))))

(defun mural-read-tag ()
  (setq ido-dynamic-match-fn 'mural-query)
  (unwind-protect ;; i have no idea what's going on here
      (ido-completing-read "tag: " '("nope"))
    (setq ido-dynamic-match-fn nil)))

(defun mural-open-dwim ()
  (interactive)
  (find-tag (mural-read-tag)))

(defadvice ido-set-matches (before ido-dynamic-match ())
  (if ido-dynamic-match-fn
      (setq ido-cur-list (funcall ido-dynamic-match-fn ido-text))))

(ad-activate 'ido-set-matches)

(provide 'mural)

