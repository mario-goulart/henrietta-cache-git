(module henrietta-cache-update-latest-versions ()

(import scheme)
(cond-expand
 (chicken-4
  (import chicken scheme)
  (use data-structures extras files ports posix setup-api))
 (chicken-5
  (import (chicken base)
          (chicken file)
          (chicken format)
          (chicken pathname)
          (chicken process-context)
          (chicken sort))
  (include "version.scm"))
 (else "Unsupported CHICKEN version."))

;; For setup-api's copy-file
(cond-expand
 (chicken-4 (run-verbose #f))
 (else (void)))

(define (update-latest-versions cache-dir git-dir)
  (for-each
   (lambda (egg)
     (let ((cache-egg-path (make-pathname cache-dir egg))
           (git-egg-path (make-pathname git-dir egg)))
       (if (file-exists? git-egg-path)
           (let ((cache-egg-versions (directory cache-egg-path))
                 (git-egg-version (car (directory git-egg-path))))
             (for-each
              (lambda (version)
                (let ((git-egg-version-path (make-pathname git-egg-path
                                                           version)))
                  (when (and (not (file-exists? git-egg-version-path))
                             (not (string=? git-egg-version version)) ; no version>?
                             (version>=? version git-egg-version))
                    (copy-file (make-pathname cache-egg-path version)
                               git-egg-version-path))))
              cache-egg-versions))
           (let* ((latest-version
                   (car (sort (directory cache-egg-path) version>=?)))
                  (git-egg-version-path
                   (make-pathname git-egg-path latest-version)))
             (create-directory git-egg-version-path 'recursively)
             (copy-file (make-pathname cache-egg-path latest-version)
                        git-egg-version-path)))))
   (directory cache-dir)))


(define (usage #!optional exit-code)
  (let ((port (if (and exit-code (not (zero? exit-code)))
                  (current-error-port)
                  (current-output-port)))
        (this (pathname-strip-directory (program-name))))
    (fprintf port
             "Usage: ~a <henrietta-cache dir> <henrietta-cache git dir>"
             this)
    (when exit-code
      (exit exit-code))))


(let ((args (command-line-arguments)))
  (when (or (null? args)
            (null? (cdr args)))
    (usage 1))
  (update-latest-versions (car args) (cdr args)))

) ;; end module
