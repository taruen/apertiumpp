#lang racket

; Setup .circleci in a monolingual Apertium linguistic data package.
; WARNING: replaces contents of config.yml and cov.sh if existent.
; USAGE: racket setup.rkt

(require rash)
(require "../turkic-repos.rkt")

(define APERTIUM-ALL "../../../apertium-all/")

(for ([l MONOLINGUAL])
  (define lang (regexp-replace #rx"/" (first (regexp-match #rx".../$" l)) ""))
  (define dir (string-append APERTIUM-ALL l))
  (display dir)(newline)
  (parameterize ([current-directory (string->path dir)])
    (display (rash "git checkout master"))
    (display (rash "git pull"))
    (display (rash "mkdir -p .circleci")))
  (display-to-file
   (rash "racket monoling-circleci-yaml.rkt (values lang)")
   (string->path (string-append dir ".circleci/config.yml"))
   #:exists 'replace)
  (display-to-file
   (rash "racket monoling-cov-sh.rkt (values lang)")
   (string->path (string-append dir ".circleci/cov.sh"))
   #:exists 'replace)
  (parameterize ([current-directory (string->path dir)])
    (display (rash "git status"))
    (display (rash "git add .circleci/config.yml .circleci/cov.sh"))
    (display (rash "git commit -m \"setup circleci\""))
    (display (rash "git push origin master"))))
  