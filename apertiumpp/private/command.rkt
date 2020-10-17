#lang racket

;; TODO fetch texts not from master but from tags, version of which matches the version of apertiumpp (backwards compatibility)
;; TOOD mv private/command/ cli.rkt
;; TODO handle all communication with the user through a ui function. All functions here should
;;      be side-effect free (i.e. return a list or stream of things and do not print anything)

(require net/url
         json)

(module+ raco
  (define command-name (with-handlers ([exn:fail? (λ (exn) #f)])
                         (vector-ref (current-command-line-arguments) 0)))
  (dispatch command-name))

(define (dispatch command-name)
  (define dispatch-thunk
    (λ ()
      (case command-name
        [(#f "help") (displayln "See https://taruen.com/apertiumpp/ for documentation.")]
        [("corpus") (handle-corpus)])))
  (dispatch-thunk))

;; Void -> Void
(define (handle-corpus)

  (define lang (make-parameter "tat"))
  
  (define parsed-args
    (command-line
     #:program "raco apertiumpp corpus"
     #:argv (vector-drop (current-command-line-arguments) 1)
     #:once-each
     [("-l" "--lang") l "ISO-639-3 code of the language to get corpora for."
                      (lang l)]
     #:args other-args other-args))

  (match parsed-args
    [(list) (corpora (lang))]
    [(list corpus-name ...)
     (corpora (string-append (lang) "/" (string-join corpus-name "/")))]))


;; String -> Void
;; - given ISO-639-3 code of the language, list corpora that we have for it
;; - given a path starting with the ISO-639-3 code of the language, e.g.
;;   eng/bible.com/ list the bible texts that we have for eng
;; - if path points to a file and not a directory, return the lines from that
;;   file
(define (corpora lang-or-path)
  (define ls
    (call/input-url (string->url (string-append "https://api.github.com/repos/taruen/apertiumpp-corpora/contents/" lang-or-path)) ;; TODO include v3 header
                    get-pure-port
                    read-json))
  (cond
    [(hash? ls) (copy-port
                 (get-pure-port
                  (string->url
                   (string-append "https://raw.githubusercontent.com/taruen/apertiumpp-corpora/6dc5da80699d6f7c67df2b763ec1cd7a1b96dc54/" lang-or-path)))
                 (current-output-port))]
    [else
     (for ([d ls])
       (displayln (hash-ref d 'name)))]))
