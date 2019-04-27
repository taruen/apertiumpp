#lang racket

;; Traverse https://api.github.com/orgs/apertium/repos and generate a Scribble
;; page with "repo name" : "travisci status image" "circleci status image"

(require net/url
         json)

(define TRAVIS-URL "https://travis-ci.org/")
(define CIRCLE-URL "https://circleci.com/gh/")
(define OUT "/tmp/outf")

(for ([p (range 1 19)]) ; curl -I "https://api.github.com/orgs/apertium/repos" => latest
  (define repos
    (call/input-url (string->url (string-append "https://api.github.com/orgs/apertium/repos?page=" (number->string p)))
                    get-pure-port
                    port->string))

  (for ([repo (string->jsexpr repos)])
    (define name (hash-ref repo 'full_name))
    (display-to-file (string-append name
                                    " : @(status-badge \"" TRAVIS-URL name ".svg?branch=master\") "
                                    "   @(status-badge \"" CIRCLE-URL name "/tree/master.svg?style=svg\")\n\n") OUT #:exists 'append)))
