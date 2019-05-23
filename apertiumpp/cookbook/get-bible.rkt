#lang racket

;; A script to downlaod Bible translations from bible.com

(require net/url
         html-parsing
         sxml/sxpath)

(define ROOT "https://www.bible.com")
(define LANGS-PAGE (string-append ROOT "/languages"))

(struct verse (id content))
;; a Verse is (verse String String)
;; interp. a single verse from the Bible
(define V-0
  (verse "GEN.1.1" "In the beginning God created the heaven and the earth."))

;; (listof String) -> Void
;; given language codes, fetch translations from the LANGS-PAGE/[lang] and
;; store each translation in the [lang] directory, one verse per line
(define (main langs)
  (void))

;; String -> (listof String)
;; return language codes for which Bible translation are available
(define (langs langs-page)
  (map (λ (s) (string-replace s "/languages/" ""))
       (filter (λ (url) (string-contains? url "/languages/"))
               (urls (fetch langs-page)))))

;; String -> (listof String)
;; given a lang code, return urls pointing to different versions in that lang
(define (versions lang)
  (map (λ (s) (string-append ROOT s))
       (filter (λ (url) (string-contains? url "/versions/"))
               (urls (fetch (string-append LANGS-PAGE "/" lang))))))

;; String -> (listof Verse)
;; given a 'versions' page url pointing to a translation, e.g.
;;   https://www.bible.com/versions/1-kjv-king-james-version
;; return a list of verses from that translation
(define (verses version)
  (define firstpage
    (fetch
     (string-append
      ROOT
      (first ((sxpath '(@ href *text*))
              ((sxpath "//a[contains(text(), 'Read Version:')]") (fetch version)))))))

  (define (recur p acc)
    (begin
      (displayln ((sxpath '(html head title)) p))
      (define nextpage
        ((sxpath '(@ href *text*))
         ((sxpath "//a[@data-vars-event-action='Next']") p)))
      (if (empty? nextpage)
          acc
          (recur (fetch (string-append ROOT (first nextpage))) (append acc ((sxpath "//span[contains(@class, 'verse')]") p))))))

  (recur firstpage '()))
 
;; utilities

;; String -> SXML
;; return contents of a webpage as sxml
(define (fetch url)
  (html->xexp (port->string (get-pure-port (string->url url)))))

;; SXML -> (listof String)
;; return all links from a page
(define (urls page)
  ((sxpath '(// @ href *text*)) page))