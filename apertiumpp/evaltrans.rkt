#lang web-server/insta

;;;;;;;;;;;;;;;;;;;
;; Data definitions

(struct tu (sl tl))
;; TU is a (tu String String)
;; interp. a translation unit = a pair of a source language string
;; and its translation into targer language

(define TU-1 (tu "сәлем" "сәлам"))
(define TU-2 (tu "Кішкентай ханзада" "Кечкенә ханзадә"))


;;;;;;;;;;;;
;; Functions


(define (start request)
  (response/xexpr
   '(html
     (head (title "Postediting `The Little Prince'"))
     (body (h1 "Сәлем!")))))