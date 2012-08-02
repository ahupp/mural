;;
;; mural adds a fast, fuzzy tag search typeahead to emacs.
;;
;; Features:
;;
;;   -- fuzzy search means that you can type any sequence of letters
;;      in a tag.  e.g, 'prof_get_s' will find 'profile_get_short'.
;;      This lets you find tags who's name you only vagely remember,
;;      is robust to some typos, and only requires typing a fraction
;;      of the characters in most case (e.g
;;      SimpleBeanFactoryAwareAspectInstanceFactory could by found by
;;      just typing SBFAAIF, or some subset thereof)
;;
;;   -- results are refined on each keypress, which lets you stop as
;;      soon as an unambiguous result is found.
;;
;;   -- it's dramatically faster than pure emacs solutions for large
;;      tags files.  Single queries return in 35ms, compared to
;;      several seconds to do a tab completion in find-tag and over
;;      10s for a fuzzy match in ido-mode.
;;
;;   -- supports multiple tag files and repositories.  The repo to
;;      search is chosen based on the location of the file in the
;;      current buffer.
;;
;;   -- changes to the tags file are picked up without a restart using inotify.
;;
;;
;; To use, put this in your .emacs:
;;
;;   (require 'mural)
;;   (mural-add-tagfile "~/www/TAGS")
;;   (mural-add-tagfile "~/local/hphp/src/TAGS")
;;   (global-set-key (kbd "C-o") 'mural-open-dwim)
;;
;; To debug launch:
;; (call-process "mural_server" nil t nil (expand-file-name "~/www/TAGS"))


(require 'ido)
(require 'cl)

(defvar mural-server-path "mural_server"
  "Path to the mural_server executable")

(defvar mural-server-echo nil
  "echo server responses to *muralserver* buffer")

(defvar ido-dynamic-match-fn nil)
(defvar mural-tagfile-to-process '()
  "alist from tagfile name to a mural_server process that handles this tagfile")

(defvar mural-current-tagfile nil)

(defun mural-add-tagfile (tagfile)
  "Register tagfile and make it available for queries"
  (mural-get-server (expand-file-name tagfile)))

(defun mural-add-tag-process-entry (tagfile process)
  "Add or replace the process associated with tagfile"
  (setq mural-tagfile-to-process
        (cons
         (cons tagfile process)
         (delete-if-not (lambda (entry)
                   (not (equal tagfile (car entry))))
                 mural-tagfile-to-process))))

(defun mural-get-server (tagfile)
  "Return the process for the given tagfile, if necessary creating/restarting it"
  (let ((proc (cdr (assoc tagfile mural-tagfile-to-process))))
    (if (or (not proc)
            (not (eq (process-status proc) 'run)))
        (let ((newproc (mural-start-server tagfile)))
          (mural-add-tag-process-entry tagfile newproc)
          newproc)
      proc)))

(defun mural-start-server (tagfile)
  "fork a new mural_server process with this tagfile, returning the process"
  (if (not tagfile)
      (error "tagfile not provided"))
  (message "Starting mural server")
  (let ((proc (start-process
               "mural" "*muralserver*"
               mural-server-path tagfile)))
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

(defun mural-tagfile-for-filename (filename)
  "Given a source file, return the name of the associated
tagfile.  This matches based on sharing the same parent
directory, so /home/bob/project/foo.c will match the tag file in
/home/bobo/project/TAGS.  If no match is found, return nil"
  (let ((resolved-fname (file-truename filename)))
    (car (delete-if-not
          (lambda (tagfile)
            ;; file-truename resolves symlinks so we make sure to
            ;; compare the same thing
            (eq 0 (string-match
                   (file-truename (file-name-directory tagfile))
                   resolved-fname)))
          (mapcar 'car mural-tagfile-to-process)))))

(defun mural-query (query tagfile)
  "Run a fuzzy QUERY against TAGFILE and return the results as
specified in mural-parse-response"
  (if (equal "" query)
      '()
    (let ((proc (mural-get-server tagfile))
          (mural-server-output ""))
      (process-send-string (process-name proc)
                           (concat query "\n"))

      ;; Wait up to 1/2 second
      (loop for i from 1 to 10
            until (mural-response-complete mural-server-output)
            do (accept-process-output proc .05))
      (if (mural-response-complete mural-server-output)
          (mural-parse-response tagfile mural-server-output)
        (error "mural-server timeout"))
      )))

(defun mural-query-save-tag (query tagfile)
  "Run by ido-mode to refresh results on keypress.  Runs
QUERY (the current contents of the minibuffer) against
mural-current-tagfile, which was setup on entry to
ido-completing-read"
  (let ((qresult (mural-query query tagfile)))
    (setq mural-last-query-result qresult)
    (mapcar 'mural-tag-tagname qresult)))

(defun mural-parse-response (tagfile results)
  "RESULTS is a tab-delimited string of the format
    MATCH tag file row
    ...
    DONE ...

mural-parse-response parses this into '((tag file-path row tagfile-path) ...).
Filenames are resolved relative to TAGFILE"
  (delq nil
        (mapcar
         (lambda (x)
           (let ((row (split-string x "\t")))
             (if (equal (elt row 0) "MATCH")
                 ;; MATCH tag filename row
                 (list (elt row 1)
                       (elt row 2)
                       (string-to-number (elt row 3))
                       tagfile)
               ;; hopefully DONE
               nil)))
         (split-string results "\n"))))

(defun mural-response-complete (result)
  "Returns t if RESULT is a complete response from the server"
  (integerp (string-match "^DONE" result)))

(defun mural-infer-tagfile ()
  (let* ((file-base (buffer-file-name))
         (tagfile-for-buffer (and file-base
                                  (mural-tagfile-for-filename file-base))))
    ;; try 1. current repo, 2. last repo used, 3. whatever is configured
    (let ((tagfile (or
                    tagfile-for-buffer
                    mural-current-tagfile
                    (if (eq 1 (length mural-tagfile-to-process))
                        (car (car mural-tagfile-to-process))))))
      (if (not tagfile)
          (error "unable to infer tagfile")
        (setq mural-current-tagfile tagfile)))))


(defun mural-read-tag (tagfile)
  "Run the typeahead in the minibuffer against a tagset
appropriate for FILE-BASE.  Return a taginfo list which can be
accessed through the mural-tag-* functions"
  (let ((ido-dynamic-match-fn (lambda (query)
                                (mural-query-save-tag query tagfile)))
        (mural-last-query-result nil)) ;; set by mural-query-save-tag
    (assoc
     (ido-completing-read "tag: " '("nope"))
     mural-last-query-result)))

;; Tags are lists like '(tag filename row tagfile)
(defun mural-tag-tagname (tag)
  (elt tag 0))
(defun mural-tag-filename (tag)
  (file-truename
   (concat
    (file-name-directory (elt tag 3))
    (elt tag 1))))
(defun mural-tag-row (tag)
  (elt tag 2))

(defun mural-open-dwim ()
  "Interactively search for a tag in the current repository and
open it in a new buffer"
  (interactive)
  (let ((taginfo (mural-read-tag (mural-infer-tagfile))))
    (find-file (mural-tag-filename taginfo))
    (goto-line (mural-tag-row taginfo))))

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

