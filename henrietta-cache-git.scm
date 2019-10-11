(module henrietta-cache-git ()

(import scheme)
(cond-expand
 (chicken-4
  (import chicken scheme)
  (use data-structures extras files posix setup-api srfi-1 srfi-13 utils))
 (chicken-5
  (import (chicken base)
          (chicken condition)
          (chicken file)
          (chicken file posix)
          (chicken format)
          (chicken io)
          (chicken pathname)
          (chicken process)
          (chicken process-context)
          (chicken sort)
          (chicken string))
  (import srfi-1 srfi-13)
  (include "version.scm"))
 (else "Unsupported CHICKEN version."))

(define *all-versions?* #f)

(define (list-directories dir)
  (filter-map (lambda (file)
                (and (directory? (make-pathname dir file))
                     file))
              (directory dir)))

(define (remove-old-versions! #!optional using-git)
  (let ((eggs (list-directories "."))
        (removed '()))
    (for-each
     (lambda (egg)
       (let* ((versions (sort (list-directories egg) version>=?))
              (versions-to-remove (cdr versions)))
         (unless (null? versions-to-remove)
           (for-each
            (lambda (version)
              (let ((to-remove (make-pathname egg version)))
                (if using-git
                    (handle-exceptions exn
                      (delete-directory to-remove 'recursively)
                      (begin
                        (system* (sprintf "git rm -rf ~a 2>/dev/null" (qs to-remove)))
                        (set! removed (cons to-remove removed))))
                    (begin
                      (delete-directory to-remove 'recursively)
                      (set! removed (cons to-remove removed))))))
            versions-to-remove))))
     eggs)
    removed))

(define (remove-inner-git-dirs!)
  ;; Some eggs pack inner git directories, which causes problem with
  ;; henrietta-cache-git, so we just remove them.
  (for-each
   (lambda (egg)
     (find-files egg
                 dotfiles: #t
                 test: (lambda (path)
                         ;; redis has a .git _file_...
                         ;; (redis/2.0/hiredis/.git)
                         (equal? ".git" (pathname-strip-directory path)))
                 action: (lambda (path _)
                           (if (directory? path)
                               (delete-directory path 'recursively)
                               (delete-file path)))))
   (directory ".")))

(define (list-new-files)
  (with-input-from-pipe "git ls-files --others --exclude-standard"
    read-lines))

(define (file-list->egg+version file-list)
  (let ((egg+version
         (map (lambda (path)
                (let ((parts (string-split path "/"))) ;; FIXME: unix-only
                  (string-append (car parts) "/" (cadr parts))))
              file-list)))
    (delete-duplicates egg+version string=?)))

(define (git-init!)
  (when (directory-exists? ".git")
    (error 'git-init! "Directory already initialized"))
  (remove-inner-git-dirs!)
  (system* "git init")
  (unless *all-versions?*
    (remove-old-versions!))
  (system* "git add .")
  (system* "git commit -a -m 'Importing'"))

(define (git-update!)
  (unless (directory-exists? ".git")
    (error 'git-update! "Directory has not been initialized."))
  (remove-inner-git-dirs!)
  (let ((removed-versions ; "<egg>/<version>"
         (if *all-versions?*
             '()
             (remove-old-versions! 'using-git)))
        (new-files (list-new-files)))
    (for-each (lambda (file)
                (system* (sprintf "git add ~a" (qs file))))
              new-files)
    (with-output-to-pipe "git commit -F -"
      (lambda ()
        (unless (null? new-files)
          (print "New versions added:")
          (for-each print (file-list->egg+version new-files)))
        (print "")
        (unless (null? removed-versions)
          (print "Old versions removed:")
          (for-each print removed-versions))))))

(define (usage #!optional exit-code)
  (let ((port (if (and exit-code (not (zero? exit-code)))
                  (current-error-port)
                  (current-output-port)))
        (this (pathname-strip-directory (program-name))))
    (fprintf port
             "Usage: ~a <command> [--all-versions] <egg sources dir>

Commands:

initialize:
    Initialize a git repository in <egg sources dir>.  If --all-versions
    is provided, then all egg versions will be added to the git repository.

update:
    Update a previously initialized git repository with the changes in
    <egg sources dir>, respecting --all-versions.  In other words, it
    handles the untracked files in <egg sources dir>.  If --all-versions
    is provided, the untracked files are added to the repository.  If
    --all-versions is not provided, the older versions are removed.
"
             this)
    (when exit-code
      (exit exit-code))))


(let* ((args (command-line-arguments))
       (non-options (remove (lambda (arg)
                              (string-prefix? "--" arg))
                            args)))
  (when (or (null? non-options)
            (null? (cdr non-options)))
    (usage 1))

  (set! *all-versions?* (and (member "--all-versions" args) #t))

  (let ((eggs-dir (cadr non-options))
        (command (car non-options)))

    (change-directory eggs-dir)

    (case (string->symbol command)
      ((initialize) (git-init!))
      ((update) (git-update!))
      (else (error 'main "Invalid command" command)))))

) ;; end module
