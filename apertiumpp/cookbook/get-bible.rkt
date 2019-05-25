#lang racket

;; A script to download Bible translations from bible.com

(require net/url
         html-parsing
         sxml/sxpath)
(module+ test
  (require rackunit))

(define ROOT "https://www.bible.com")
(define LANGS-PAGE (string-append ROOT "/languages"))
(define REQUEST-HEADERS
  '("User-agent: Mozilla/5.0 (compatible; Taruenbot/0.1; +http://taruen.com/apertiumpp/)"))
(define SLEEP 1) ;; second(s) between requests

(struct verse (id content) #:transparent)
;; a Verse is (verse String String)
;; interp. a single verse from the Bible
(define V-0
  (verse "GEN.1.1" "In the beginning God created the heaven and the earth."))

;; (listof String) -> Void
;; given language codes, fetch translations from the LANGS-PAGE/[lang] and
;; store each translation in the [lang] directory, one verse per line
(define (main langs)
  (for ([lang langs])
    (for ([version (versions lang)])
      (for ([verse (verses version)])
        (displayln verse)))))


;(define outf (string-append lang "/" (id version) ".csv"))

;      (call-with-output-file outf
;        (λ (out)
;          (for ([v (verses version)])
;            (displayln (string-append (verse-id v) "\t" (verse-content v)))))))))

;; String -> (listof String)
;; given LANGS-PAGE, return language codes for which
;; a Bible translation is available
(define (langs langs-page)
  (map (λ (s) (string-replace s "/languages/" ""))
       (filter (λ (url) (string-contains? url "/languages/"))
               (urls (fetch langs-page)))))

;; String -> (listof String)
;; given a lang code, return urls of available Bible versions in that lang
(define (versions lang)
  (map (λ (s) (string-append ROOT s))
       (filter (λ (url) (string-contains? url "/versions/"))
               (urls (fetch (string-append LANGS-PAGE "/" lang))))))

;; String -> (listof Verse)
;; given a 'versions' page url, e.g.
;;   https://www.bible.com/versions/1-kjv-king-james-version
;; return a list of verses from that translation
(define (verses version)

  (define (recur page accum)
    (begin
      (displayln ((sxpath '(html head title)) page))
      (sleep SLEEP)
      (define nextpage (next page))
      (if nextpage
          (recur (fetch nextpage) (append accum (scrap page)))
          (append accum (scrap page)))))

  (recur (page1 version) '()))

;; String -> SXML
;; given a 'versions' page url, e.g.
;;   https://www.bible.com/versions/1-kjv-king-james-version
;; return the first page of that partucular Bible translation
(define (page1 version)
  (fetch
   (string-append
    ROOT
    (first ((sxpath '(@ href *text*))
            ((sxpath "//a[contains(text(), 'Read Version:')]")
             (fetch version)))))))

;; SXML -> (or/c String #f)
;; given a page of Bible translation, return the url of the next page
(define (next page)
  (define n
    ((sxpath '(@ href *text*))
     ((sxpath "//a[@data-vars-event-action='Next']") page)))
  (if (empty? n)
      #f
      (string-append ROOT (first n))))

;; SXML -> (listof Verse)
;; given a page of Bible translation, return verses from it
(define (scrap page)
  (filter
   has-content?
   (map
    span->verse
    ((sxpath "//span[contains(@class, 'verse')]") page))))

;; SXML -> Verse
;; given a <span class="verse"> element, return a verse struct
(define (span->verse span)
  (define id (first ((sxpath '(@ data-usfm *text*)) span)))
  (define con
    ((sxpath "span[contains(@class, 'content')]/text()") span))
  (if (empty? con)
      (verse id "")
      (verse id (first con))))
      
;; Verse -> Boolean
;; return #t if verse has real content
(define (has-content? v)
  (not (regexp-match #rx"^ *$" (verse-content v))))
  
;; utilities

;; String -> SXML
;; return contents of a webpage as sxml
(define (fetch url)
  (html->xexp
   (port->string (get-pure-port (string->url url) REQUEST-HEADERS))))

;; SXML -> (listof String)
;; return all links from a page
(define (urls page)
  ((sxpath '(// @ href *text*)) page))

;; String -> String
;; remove base url, leaving only version id
(define (id version)
  (string-replace version (string-append ROOT "/versions/") ""))
(module+ test
  (check-equal? (id "https://www.bible.com/versions/1929-bsknt14-inzhil")
                "1929-bsknt14-inzhil"))