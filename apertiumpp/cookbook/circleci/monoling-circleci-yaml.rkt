#lang scribble/text

@; Generate a .circleci/config.yml for a monolingual Apertium package.
@; USAGE: racket monoling-circleci-yaml.rkt <ISO 639-3 language code>
@; EXAMPLE: racket monoling-circleci-yaml.rkt tat

@(define LANG (vector-ref (current-command-line-arguments) 0))

version: 2.0
jobs:
  build:
    docker:
      - image: apertium/base:release
    steps:
      - checkout

      - run: ./autogen.sh
      - run: make

      - run:
          name: Install apertium-quality and awscli
          command: |
            apt-get -y install python3 python3-setuptools python3-dev python3-yaml curl python-pip
            git clone https://github.com/apertium/apertium-quality.git
            cd apertium-quality/mwtools/python3
            python3 ./setup.py install
            cd ../../
            ./autogen.sh && make && make install
            cd ../../../
            pip install awscli

      - run:
          name: Measure coverage
          command: |
            mkdir /tmp/res/
            curl https://raw.githubusercontent.com/taruen/apertiumpp/master/data4apertium/corpora/jam/@|LANG|.txt > /tmp/jam.@|LANG|.txt
            sh cov.sh jam
            curl https://raw.githubusercontent.com/taruen/apertiumpp/master/data4apertium/corpora/udhr/udhr_@|LANG|.txt > /tmp/udhr.@|LANG|.txt
            sh cov.sh udhr
            curl https://raw.githubusercontent.com/taruen/apertiumpp/master/data4apertium/corpora/bible/@|LANG|.nt.ibt.txt > /tmp/bible.@|LANG|.txt
            sh cov.sh bible
            aws s3 cp /tmp/res/ s3://apertium/apertium-@|LANG|/ --exclude "*" --include "cov.*.txt"
            aws s3 cp /tmp/res/ s3://apertium/apertium-@|LANG|/ --exclude "*" --include "cov.*.svg"

      - store_artifacts:
          path: /tmp/res/