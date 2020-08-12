#lang racket/base

(require "scgi/private/scgi.rkt" "scgi/private/mime-types.rkt" "scgi/private/http-status-codes.rkt"
         net/cookies/server racket/contract racket/tcp racket/unix-socket gregor xml json)

(define scgi-responder/c (-> scgi-request? input-port? output-port? any/c))
(define scgi-handler/c (-> scgi-request? any/c))
(define scgi-websock-handler/c (-> scgi-ws-conn? any/c any/c))

(provide/contract
 [scgi-serve
  (->* (scgi-responder/c)
       (#:port port-number? #:max-allow-wait exact-positive-integer? #:reuse? any/c #:hostname (or/c string? #f))
       any/c)]
 [scgi-serve/unix-socket
  (->* (scgi-responder/c unix-socket-path?)
       (#:max-allow-wait exact-positive-integer?)
       any/c)]
 [make-response
  (->* (string? (or/c string? bytes?))
       (#:status status-code? #:date moment? #:cookies (listof cookie?) #:headers (listof bytes?))
       scgi-response?)]
 [make-response/xexpr
  (->* (xexpr?)
       (#:content-type string? #:status status-code? #:date moment? #:cookies (listof cookie?) #:headers (listof bytes?))
       scgi-response?)]
 [make-response/text
  (->* (string?)
       (#:content-type string? #:status status-code? #:date moment? #:cookies (listof cookie?) #:headers (listof bytes?))
       scgi-response?)]
 [make-response/json
  (->* (jsexpr?)
       (#:content-type string? #:status status-code? #:date moment? #:cookies (listof cookie?) #:headers (listof bytes?))
       scgi-response?)]
 [output-response
  (-> scgi-response? output-port? any/c)]
 [make-scgi-responder (-> scgi-handler/c scgi-responder/c)]
 [make-websock-responder (->* (scgi-websock-handler/c)
                              (#:on-connect (or/c #f (-> scgi-request? (values (listof bytes?) any/c))))
                              scgi-responder/c)]
 [scgi-websocket-close! (-> scgi-ws-conn? any/c)]
 [scgi-websocket-read! (-> scgi-ws-conn? (or/c bytes? string? eof-object?))]
 [scgi-websocket-send! (-> scgi-ws-conn? (or/c bytes? string?) any/c)]
 )

(provide
         scgi-request-post-data scgi-request-cookies scgi-request-cookie-assf
         scgi-request? scgi-request-method scgi-request-scheme scgi-request-host scgi-request-uri scgi-request-content scgi-request-headers
         (struct-out scgi-response)
         (struct-out scgi-ws-conn)
         scgi-ws-connection-timeout)

(provide status-code? status->message scgi-handler/c scgi-responder/c scgi-websock-handler/c
         (prefix-out MIME: (all-from-out "scgi/private/mime-types.rkt")))


 