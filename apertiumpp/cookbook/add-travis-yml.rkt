#lang racket

; Add .travis.yml file to an Apertium linguistic data package if it's missing
; By default, Travis will only `make' the package. It won't run `make test'
; or `make check` or anything else.

(require rash)
(require "turkic-repos.rkt")

(define MONOLINGUAL-TRAVIS-YML
  #<<end
dist: xenial
before_install:
    - wget http://apertium.projectjj.com/apt/install-nightly.sh -O - | sudo bash
    - sudo apt-get install hfst apertium lttoolbox apertium-dev lttoolbox-dev libhfst-dev cg3 python3-libhfst
    - rm tests/morphophonology/test.py
    - wget https://raw.githubusercontent.com/apertium/apertium-tat/master/tests/morphophonology/test.py -O tests/morphophonology/test.py
script:
    - ./autogen.sh
    - make
#    - make check
notifications:
    irc:
        channels:
            - "chat.freenode.net#apertium"
        on_failure: change
        on_success: never
    email:
        recipients:
            - ilnar@selimcan.org
        on_failure: always
        on_success: always
end
  )

;(for ([l MONOLINGUAL])
;  (define travisyml (string-append "apertium-all/" l ".travis.yml"))  
;  (when (not (file-exists? travisyml))
;    (display-to-file MONOLINGUAL-TRAVIS-YML travisyml)))
; git pull
; etc