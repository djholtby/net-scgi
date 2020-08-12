#lang racket/base

(require net/url net/uri-codec (except-in net/cookies parse-date) racket/tcp racket/list racket/random
         racket/string racket/promise racket/format gregor xml json net/base64 racket/unix-socket
         "http-status-codes.rkt" (prefix-in MIME: "mime-types.rkt") "mime-parse.rkt")

(provide scgi-serve scgi-serve/unix-socket
         make-response
         make-response/xexpr
         make-response/text
         make-response/json
         output-response
         make-scgi-responder make-websock-responder
         
         scgi-request-post-data scgi-request-cookies scgi-request-cookie-assf
         (prefix-out scgi- (combine-out request? request-method request-scheme request-host request-uri request-content request-headers))
         (prefix-out scgi- (struct-out response))
         (prefix-out scgi- (struct-out ws-conn))
         (prefix-out scgi- (combine-out websocket-send! websocket-read! ws-connection-timeout websocket-close!))
         )



(struct request
  (method scheme host uri cookies post-data content headers) #:transparent)

(define (maybe-bytes->string mb)
  (and mb (bytes->string/utf-8 mb)))

(define (make-scgi-request headers body-promise)
  ;(pretty-print headers (current-error-port))
  (request
   (maybe-bytes->string (hash-ref headers 'REQUEST_METHOD #f))
   (if (bytes=? (hash-ref headers 'HTTPS #"") #"") "HTTP" "HTTPS")
   (maybe-bytes->string (hash-ref headers 'HTTP_HOST #f))
   (string->url (bytes->string/utf-8 (hash-ref headers 'REQUEST_URI)))
   (delay (cookie-header->alist (hash-ref headers 'COOKIE #"")))
   (delay (parse-form-bindings headers (force body-promise)))
   (cons (hash-ref headers 'CONTENT_TYPE #f)
         body-promise)
   headers))

(define (scgi-request-post-data req)
  (force (request-post-data req)))

(define (scgi-request-cookies req)
  (force (request-cookies req)))

(define (scgi-request-cookie-assf req match?)
  (assf (force (request-cookies req))
        match?))

(struct response
  (status date content-type cookies other-headers content) #:transparent)


(define (make-response content-type content
                       #:status [status 200] #:date [date (now/moment/utc)] #:cookies [cookies '()] #:headers [headers '()])
  (response status date content-type cookies headers content))

(define (make-response/xexpr x #:content-type [content-type MIME:HTML]
                             #:status [status 200] #:date [date (now/moment/utc)] #:cookies [cookies '()] #:headers [headers '()])
  (response status date content-type cookies headers (xexpr->string x)))

(define (make-response/text t #:content-type [content-type MIME:TEXT]
                            #:status [status 200] #:date [date (now/moment/utc)] #:cookies [cookies '()] #:headers [headers '()])
  (response status date content-type cookies headers (~a t)))

(define (make-response/json jx #:content-type [content-type MIME:JSON]
                            #:status [status 200] #:date [date (now/moment/utc)] #:cookies [cookies '()] #:headers [headers '()])
  (response status date content-type cookies headers (jsexpr->string jx)))

(define (->http-time timestamp)
  (~t (adjust-timezone timestamp "GMT") "E, MMMM d, y, HH:mm:ss zzz"))



(define (output-response resp out)
  (let ([content/bytes (if (bytes? (response-content resp)) (response-content resp) (string->bytes/utf-8 (response-content resp)))])
    (fprintf out "Status: ~a ~a" (response-status resp) (status->message (response-status resp)))
    (write-bytes <CRLF> out)
    (when (response-content-type resp)
      (fprintf out "Content-Type: ~a" (response-content-type resp))
      (write-bytes <CRLF> out))
    (fprintf out "Date: ~a" (->http-time (response-date resp)))
    (write-bytes <CRLF> out)
    (fprintf out "Content-Length: ~a" (bytes-length content/bytes))
    (write-bytes <CRLF> out)
    (for ([cookie (in-list (response-cookies resp))])
      (write-bytes #"Set-Cookie: " out)
      (write-bytes (cookie->set-cookie-header cookie) out)
      (write-bytes <CRLF> out))
    (for ([header (in-list (response-other-headers resp))])
      (write-bytes header out)
      (write-bytes <CRLF> out))
    (write-bytes <CRLF> out)
    (unless (zero? (bytes-length content/bytes)) (write-bytes content/bytes out))))

(define (parse-headers count in)
  (let ([bs (read-bytes count in)])
    (let loop ([i 0]
               [name-start 0]
               [name-end #f]
               [value-start #f]
               [headers (hasheq)])
      (cond [(= i count) headers]
            [(not (zero? (bytes-ref bs i)))
             (loop (add1 i) name-start name-end value-start headers)]
            [value-start
             (loop (add1 i)
                   (add1 i)
                   #f
                   #f
                   (hash-set headers
                             (string->symbol (string-upcase (bytes->string/utf-8 bs #f name-start name-end)))
                             (subbytes bs value-start i)))]
            [else
             (loop (add1 i)
                   name-start
                   i
                   (add1 i)
                   headers)]))))

(define (maybe-bytes->number bs)
  (if (cons? bs) (maybe-bytes->number (cdr bs))
      (if bs
          (let loop ([i 0]
                     [num 0])
            (if (= i (bytes-length bs))
                num
                (loop (add1 i)
                      (+ (* num 10)
                         (- (bytes-ref bs i) 48)))))
          0)))

(define (parse-request in)
  (let loop
    ([i 0])
    (let [(b (read-byte in))]
      (if (eqv? b 58) ; colon
          (let* ([headers (parse-headers i in)]
                 [comma (read-byte in)]
                 [body-promise (delay (read-bytes (maybe-bytes->number (hash-ref headers 'CONTENT_LENGTH #f)) in))])
            (make-scgi-request headers body-promise))
          (if (<= 48 b 57)
              (loop (+ (* 10 i) (- b 48)))
              (begin
                (close-input-port in)
                #f))))))

(define <CRLF> #"\r\n")

;; (->response resp) returns resp if it is already  response, otherwise coerces resp into a text/plain response (using ~a format)
(define (->response resp)
  (if (response? resp)
      resp
      (make-response/text (~a resp))))

(define (make-scgi-responder handler)
  (λ (req in out)
    (let ([resp (handler req)])
      (output-response (->response resp) out)
      (flush-output out))))

(define (scgi-serve/listener handler listener-custodian listener)
  (let ([accept (if (unix-socket-listener? listener) unix-socket-accept tcp-accept)])
      (dynamic-wind (λ () (void))
                  (λ ()
                    (let loop ()
                      (define request-custodian (make-custodian listener-custodian))
                      (parameterize ([current-custodian request-custodian])
                        (let-values ([(in out) (accept listener)])
                          (thread
                           (λ ()
                             (dynamic-wind (λ () (void))
                                           (λ ()
                                             (handler (parse-request in) in out))
                                           (λ ()
                                             (custodian-shutdown-all request-custodian)))))))
                      (loop)))
                  (λ ()
                    (custodian-shutdown-all listener-custodian)))))


(define (scgi-serve/unix-socket handler socket-path #:max-allow-wait [max-allow-wait 4])
  (define listener-custodian (make-custodian))
  (parameterize ([current-custodian listener-custodian]
                 [current-websocket-manager (make-ws-conn-manager listener-custodian)])
    (scgi-serve/listener
     handler
     listener-custodian
     (unix-socket-listen socket-path max-allow-wait))))

(define (scgi-serve handler #:port [port 4000] #:max-allow-wait [max-allow-wait 511] #:reuse? [reuse? #f] #:hostname [hostname "127.0.0.1"])
  (define listener-custodian (make-custodian))
  (parameterize ([current-custodian listener-custodian]
                 [current-websocket-manager (make-ws-conn-manager listener-custodian)]) 
    (scgi-serve/listener
     handler
     listener-custodian
     (tcp-listen port max-allow-wait reuse? hostname))))



#|

  WEBSOCK BUSINESS

|#
(define websocket-accept-uuid #"258EAFA5-E914-47DA-95CA-C5AB0DC85B11")

(struct ws-conn (in out manager server? [connected? #:mutable] [timeout #:mutable])
   #:property prop:evt (struct-field-index in))

(define (make-websock-connection in out server?)
  (let ([conn (ws-conn in out (current-websocket-manager) server? #t (+ (current-seconds) (ws-connection-timeout)))])
    (ws-manager-msg 'new conn)
    conn))


(define (ws-manager-msg cmd conn)
  (thread-send (websocket-manager-thread (ws-conn-manager conn)) (cons cmd conn)))

(define ws-connection-timeout
  (make-parameter 10 (λ (v) (unless (exact-positive-integer? v) (raise-argument-error 'websocket-timeout "exact-positive-integer?" v)) v)))


(struct websocket-manager (thread))

(define current-websocket-manager
  (make-parameter #f (λ (v) (unless (websocket-manager? v) (raise-argument-error 'current-websocket-manager "websocket-manager?" v)) v)))

(struct min-heap ([count #:mutable] [nodes #:mutable] keys))

(define minimum-heap-size 64)

(define (make-min-heap)
  (min-heap 0 (make-vector minimum-heap-size #f) (make-weak-hasheq)))

(define (min-heap-add! heap conn)
  (when (= (min-heap-count heap) (vector-length (min-heap-nodes heap)))
    (let ([old (min-heap-nodes heap)])
      (set-min-heap-nodes! heap (make-vector (* 2 (vector-length old))))
      (vector-copy! (min-heap-nodes heap) 0 old)))
  (vector-set! (min-heap-nodes heap) (min-heap-count heap) conn)
  (set-min-heap-count! heap (add1 (min-heap-count heap)))
  (let loop ([i (min-heap-count heap)])
    (unless (zero? i)
      (let ([parent (arithmetic-shift (sub1 i) -1)])
        (cond [(> (ws-conn-timeout (vector-ref (min-heap-nodes heap) parent))
                   (ws-conn-timeout conn))
                (vector-set! (min-heap-nodes heap) i (vector-ref (min-heap-nodes heap) parent))
                (hash-set! (min-heap-keys heap) (vector-ref (min-heap-nodes heap) parent) i)
                (loop parent)]
               [else
                (vector-set! (min-heap-nodes heap) i conn)
                (hash-set! (min-heap-keys heap) conn i)])))))
          

(define (min-heap-percolate-down! heap conn [index #f])
  (let loop ([i (or index (hash-ref (min-heap-keys heap) conn))])
    (let ([c1/idx (add1 (arithmetic-shift i 1))]
          [c2/idx (+ 2 (arithmetic-shift i 1))])
      (let ([c1 (and (< c1/idx (min-heap-count heap)) (vector-ref (min-heap-nodes heap) c1/idx))]
            [c2 (and (< c2/idx (min-heap-count heap)) (vector-ref (min-heap-nodes heap) c2/idx))])
      (cond [(or (not c1)
                 (and (not c2) (<= (ws-conn-timeout conn) (ws-conn-timeout c1)))
                 (and (<= (ws-conn-timeout conn) (ws-conn-timeout c1))
                      (<= (ws-conn-timeout conn) (ws-conn-timeout c2))))
             (vector-set! (min-heap-nodes heap) i conn)
             (hash-set! (min-heap-keys heap) conn i)]
            [(or (and (not c2) (> (ws-conn-timeout conn) (ws-conn-timeout c1)))
                 (>= (ws-conn-timeout conn) (ws-conn-timeout c1) (ws-conn-timeout c2)))
             (vector-set! (min-heap-nodes heap) i c1)
             (hash-set! (min-heap-keys heap) c1 i)
             (loop c1/idx)]
            [else
             (vector-set! (min-heap-nodes heap) i c2)
             (hash-set! (min-heap-keys heap) c2 i)
             (loop c2/idx)])))))

#| (min-heap-demote! heap conn) moves conn down in the heap
    requires: conn is in heap
|#
(define (min-heap-demote! heap conn)
  (when (and (ws-conn-connected? conn) (hash-has-key? (min-heap-keys heap) conn))
    (set-ws-conn-timeout! conn (+ (current-seconds) (ws-connection-timeout)))
    (min-heap-percolate-down! heap conn)))

#| (heap-check-shrink! heap tail) shrinks heap if it is less than 25% full.  If tail is specified, it is returned
     otherwise void is returned
|#

(define (heap-check-shrink! heap [tail (void)])
  (when (< minimum-heap-size (min-heap-count heap) (arithmetic-shift (vector-length (min-heap-nodes heap)) -2))
    (let ([old (min-heap-nodes heap)])
      (set-min-heap-nodes! heap (make-vector (arithmetic-shift (vector-length (min-heap-nodes heap)) -1)))
      (vector-copy! (min-heap-nodes heap) 0 old 0 (min-heap-count heap))))
  tail)

(define (min-heap-remove! heap conn)
  (let ([i (hash-ref (min-heap-keys heap) conn #f)])
    (when i
      (vector-set! (min-heap-nodes heap) i 0)
      (set-min-heap-count! heap (sub1 (min-heap-count heap)))
      (hash-remove! (min-heap-keys heap) conn)
      (when (< i (sub1 (min-heap-count heap)))
        (min-heap-percolate-down! heap (vector-ref (min-heap-nodes heap) (min-heap-count heap)) i))
      (heap-check-shrink! heap))))
      
(define (min-heap-disconnect! heap)
  (heap-check-shrink!
   heap
   (let loop ()
     (cond [(zero? (min-heap-count heap)) #f]
           [(> (ws-conn-timeout (vector-ref (min-heap-nodes heap) 0))
              (current-seconds))
            (- (ws-conn-timeout (vector-ref (min-heap-nodes heap) 0)) (current-seconds))]
          [else (let [(conn (vector-ref (min-heap-nodes heap) 0))]
                  (websocket-close! conn #"timeout")
                  (vector-set! (min-heap-nodes heap) 0 0)
                  (hash-remove! (min-heap-keys heap) conn)
                  (set-min-heap-count! heap (sub1 (min-heap-count heap)))
                  (unless (zero? (min-heap-count heap))
                    (min-heap-percolate-down! heap (vector-ref (min-heap-nodes heap) (min-heap-count heap)) 0))
                  (loop))]))))
                
(define (make-ws-conn-manager [cust (current-custodian)])
  (parameterize ([current-custodian cust])
    (let ([result-channel (make-channel)])
      (thread (λ ()
                (let ([heap (make-min-heap)]
                      [sema (make-semaphore 1)])
                  (channel-put result-channel (websocket-manager (current-thread)))
                  (let loop ()
                    (let ([sleep-amount (min-heap-disconnect! heap)])
                      (let ([sync-result (sync/timeout sleep-amount (thread-receive-evt))])
                        (when sync-result
                          (let ([msg (thread-receive)])
                            (when (pair? msg)
                              (let ([action (car msg)]
                                    [conn (cdr msg)])
                                (case action
                                  [(bump) (min-heap-demote! heap conn)]
                                  [(new) (min-heap-add! heap conn)]
                                  [(close) (min-heap-remove! heap conn)])))))
                        (loop)))))))
      (channel-get result-channel))))

(define (websocket-close! conn [reason #""])
  (ws-manager-msg 'close conn)
  (when (ws-conn-connected? conn)
    (set-ws-conn-connected?! conn #f)
    (close-input-port (ws-conn-in conn))
    (write-frame (websocket-frame #t 8 reason) (ws-conn-out conn) (not (ws-conn-server? conn)))
    (close-output-port (ws-conn-out conn))))

(define (do-pong! conn)
  (write-frame (websocket-frame #t 10 #"") (ws-conn-out conn) (not (ws-conn-server? conn))))

(define (mask-payload mask payload length)
  (when mask
    (for [(i (in-range length))
          (b (in-bytes payload))]
      (bytes-set! payload i
                  (bitwise-xor (bytes-ref mask (modulo i 4)) b))))
  payload)
                               

(struct websocket-frame (final? opcode payload) #:transparent)

(define (read-frame in)
  (with-handlers ([exn:fail? (λ (e) (if (regexp-match #rx"input port is closed" (exn-message e)) eof (raise e)))])
    (define header (read-byte in))
    (define payload-len+mask (read-byte in))
    (cond [(eof-object? payload-len+mask) eof]
          [else
           (define fin? (not (zero? (bitwise-and header #x80))))
           (define opcode (bitwise-and header #x0F))
           (define mask? (not (zero? (bitwise-and payload-len+mask #x80))))
           (define payload-length-prefix (bitwise-and payload-len+mask #x7F))
           (define payload-length (case payload-length-prefix
                                    [(127) (integer-bytes->integer (read-bytes 8 in) #f #t)]
                                    [(126) (integer-bytes->integer (read-bytes 2 in) #f #t)]
                                    [else payload-length-prefix]))
           (define mask (and mask? (read-bytes 4 in)))
           (define payload (read-bytes payload-length in))
           (if (eof-object? payload) eof (websocket-frame fin? opcode (mask-payload mask payload payload-length)))])))

(define (write-frame frame out [mask? #t])
  (with-handlers ([exn:fail? (λ (e) (if (regexp-match #rx"output port is closed" (exn-message e)) eof (raise e)))])
    (define test-buffer (open-output-bytes))
    (define mask (if mask? (crypto-random-bytes 4) #f))
    (define header (bitwise-ior (if (websocket-frame-final? frame) #x80 #x00)
                              (websocket-frame-opcode frame)))
    (write-byte header out) (write-byte header test-buffer)
  
    (define payload-length (bytes-length (websocket-frame-payload frame)))
    (define length+mask
      (bitwise-ior (if mask? #x80 #x00)
                   (cond [(< payload-length 126) payload-length]
                         [(< payload-length #xFFFF) 126]
                         [else 127])))
    (write-byte length+mask out) (write-byte  length+mask test-buffer)
    
    (cond [(< 125 payload-length #xFFFF) (write-bytes (integer->integer-bytes payload-length 2 #f #t) out) (write-bytes (integer->integer-bytes payload-length 2 #f #t) test-buffer)]
          [(>= payload-length #xFFFF) (write-bytes (integer->integer-bytes payload-length 8 #f #t) out) (write-bytes (integer->integer-bytes payload-length 8 #f #t) test-buffer)])
    (cond [mask?
           (write-bytes mask out) (write-bytes mask test-buffer)
           (write-bytes (mask-payload mask (websocket-frame-payload frame) payload-length) out) (write-bytes (mask-payload mask (websocket-frame-payload frame) payload-length) test-buffer)]
          [else
           (write-bytes (websocket-frame-payload frame) out) (write-bytes (websocket-frame-payload frame) test-buffer)])
    ;(println (get-output-bytes test-buffer))
    (flush-output out)))
  
  
(define (websocket-send! conn message)
  (ws-manager-msg 'bump conn)
  (write-frame
   (websocket-frame #t (if (string? message) 1 2)
                    (if (string? message) (string->bytes/utf-8 message) message))
   (ws-conn-out conn)
   (not (ws-conn-server? conn))))

(define (payload-bytes->payload who opcode bs)
  (case opcode
    [(1) (bytes->string/utf-8 bs)]
    [(2) bs]
    [(3 4 5 6 7) (error who "Unsupported websocket frame opcode: ~v" opcode)]
    [(0) (error who "Received continuation frame without initial frame")]))

(define (websocket-read! conn)
  (let loop ([frame-buffer (open-output-bytes)]
             [opcode #f])
    (ws-manager-msg 'bump conn)
    (define frame (read-frame (ws-conn-in conn)))
    (cond [(eof-object? frame) (websocket-close! conn) eof]
          [(> (websocket-frame-opcode frame) 7) ; control frame
           (case (websocket-frame-opcode frame)
             [(8) (websocket-close! conn (websocket-frame-payload frame)) eof]
             [(9) (do-pong! conn) (loop frame-buffer opcode)]
             [(10 11 12 13 14 15) (loop frame-buffer opcode)])] ; reserved for future control frames [10 is pong, but there's nothing to do]
          [(websocket-frame-final? frame)
           (write-bytes (websocket-frame-payload frame) frame-buffer)
           (payload-bytes->payload 'websocket-read! (or opcode (websocket-frame-opcode frame)) (get-output-bytes frame-buffer #t))]
          [else ; not final frame, not control frame
           (write-bytes (websocket-frame-payload frame) frame-buffer)
           (loop frame-buffer (or opcode (websocket-frame-opcode frame)))])))


(define (make-websock-responder handler #:on-connect [on-connect #f])
  (λ (req in out)
    (let ([websocket-key (hash-ref (request-headers req) 'HTTP_SEC_WEBSOCKET_KEY #f)])
      (cond [(or (not websocket-key)
                 (not (bytes=? (hash-ref (request-headers req) 'HTTP_SEC_WEBSOCKET_VERSION #"") #"13"))
                 (not (string=? "websocket" (string-downcase (string-trim (bytes->string/utf-8 (hash-ref (request-headers req) 'HTTP_UPGRADE #""))))))
                 (not (string=? "upgrade" (string-downcase (string-trim (bytes->string/utf-8 (hash-ref (request-headers req) 'HTTP_CONNECTION #"")))))))
             (output-response
              (make-response #f "" #:status 400)
             out)
             (flush-output out)
             ]
            [else
             (let-values ([(extra-headers state) (if on-connect (on-connect req) (values '() req))])
               (output-response
                (make-response
                 #f "" #:status 101 #:headers `(#"Upgrade: websocket"
                                                #"Connection: Upgrade"
                                                ,(bytes-append #"Sec-WebSocket-Accept: " (base64-encode (sha1-bytes (bytes-append websocket-key websocket-accept-uuid)) #""))
                                                ,@extra-headers))
                out)
               (flush-output out)
               (handler (make-websock-connection in out #t) state))]))))
             
             




#|(module+ main
;  (require racket/pretty)
  (define f1 (websocket-frame #t 1 #"Haldo"))
  (define f1-bytes (open-output-bytes))
  (write-frame f1 f1-bytes #f)
  (define f2 (read-frame (open-input-bytes (get-output-bytes f1-bytes))))
  (writeln f2)



  (scgi-serve
   (make-websock-responder (λ (conn _ignore_)
                             (websocket-send! conn "Hi!\n")
                             (define msg (websocket-read! conn))
                             (displayln msg)
                             (websocket-send! conn msg)
                             (close-websocket conn #"That's all she wrote"))))
   #|(make-scgi-responder
    (λ (request)
      (make-response/xexpr '(html () (body () (p () "Testing testing 123!")))
                           #:cookies (list (make-cookie "my-cookie" "testing+testing+123"
                                                        #:secure? #t #:http-only? #t #:max-age 7200)))))))|#
)|#