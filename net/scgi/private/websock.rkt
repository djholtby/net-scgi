#lang racket/base

(require "scgi.rkt" racket/string racket/bytes file/sha1 net/base64 racket/random)

#|(struct request
  (method scheme host uri cookies post-data content headers) #:transparent)
|#
(require net/rfc6455)

