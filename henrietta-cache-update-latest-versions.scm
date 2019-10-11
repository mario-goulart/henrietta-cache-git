(module henrietta-cache-update-latest-versions ()

(import scheme)
(cond-expand
 (chicken-4
  (import chicken foreign scheme)
  (use data-structures extras files ports posix setup-api utils))
 (chicken-5
  (import (chicken base)
          (chicken file)
          (chicken foreign)
          (chicken format)
          (chicken pathname)
          (chicken process)
          (chicken process-context)
          (chicken sort))
  (include "version.scm"))
 (else "Unsupported CHICKEN version."))

;; For setup-api's copy-file
(cond-expand
 (chicken-4 (run-verbose #f))
 (else (void)))

(define windows-shell?
  (foreign-value "C_WINDOWS_SHELL" bool))

(define copy-command
  (if windows-shell?
      "copy"
      "cp -r"))

(define (copy-directory from to)
  (system* (sprintf "~a ~a ~a" copy-command (qs from) (qs to))))

(define (update-latest-versions cache-dir git-dir)
  (for-each
   (lambda (egg)
     (let* ((cache-egg-path (make-pathname cache-dir egg))
            (git-egg-path (make-pathname git-dir egg))
            (latest-version
             (car (sort (directory cache-egg-path) version>=?)))
            (git-egg-version-path
             (make-pathname git-egg-path latest-version)))
       (unless (directory-exists? git-egg-path)
         (create-directory git-egg-path 'recursively))
       (unless (directory-exists? (make-pathname git-egg-path latest-version))
         (copy-directory (make-pathname cache-egg-path latest-version)
                         git-egg-version-path))))
   (directory cache-dir)))


(define (usage exit-code)
  (let ((port (if (and exit-code (not (zero? exit-code)))
                  (current-error-port)
                  (current-output-port)))
        (this (pathname-strip-directory (program-name))))
    (fprintf port
             "Usage: ~a <henrietta-cache dir> <henrietta-cache git dir>\n"
             this)
    (exit exit-code)))


(let ((args (command-line-arguments)))
  (when (or (member "-h" args)
            (member "-help" args)
            (member "--help" args))
    (usage 0))
  (when (or (null? args)
            (null? (cdr args)))
    (usage 1))
  (update-latest-versions (car args) (cdr args)))

) ;; end module
