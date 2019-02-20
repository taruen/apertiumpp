#lang racket

(provide lu-contains?)

(module+ test
  (require rackunit))

  
;;;;;;;;;;;;
;; Constants


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Macros / syntax / language


;;;;;;;;;;;;;
;; Functions


(define/contract (lu-contains? lu analysis)
  (string? string? . -> . (or/c string? #f))
  ;; return `analysis' if it is among analyses of a lelxical unit, #f otherwise
  (let ([m (regexp-match (string-append "/" (regexp-quote analysis) "[/\\$]")
                         lu)])
    (if m
        (let ([a (car m)])
          (substring a 1 (- (string-length a) 1)))
        #f)))

(module+ test
  (let ([lu "қой/қой<n><nom>/қой<n><nom>+е<cop><aor><p3><sg>$"])
    (check-equal? (lu-contains? lu "қой<v><tv><imp><p2><sg>")
                  #f)
    (check-equal? (lu-contains? lu "қой<n><nom>+е<cop><aor><p3><sg>")
                  "қой<n><nom>+е<cop><aor><p3><sg>")
    (check-equal? (lu-contains? lu "қой<n><nom>")
                  "қой<n><nom>")
    (check-equal? (lu-contains? lu "қой<n><acc>")
                  #f)
    (check-equal? (lu-contains? lu "қой<n>")
                  #f)
    (check-equal? (lu-contains? lu "қой<n><nom><more>")
                  #f)))