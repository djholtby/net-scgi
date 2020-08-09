#lang racket/base

;(struct mime-type (type charset)
(require net/mime net/uri-codec racket/match charset json)

(provide parse-form-bindings parse-mimetype)

(define mime-regexp
  #px"^([a-zA-Z-]+)/([^\\s()\\[\\]?\\.<>@,;:\\\\/=[:cntrl:]]+)")
(define mime-parameter-regexp
  #px"\\s*;\\s*([^\\s()\\[\\]?\\.<>@,;:\\\\/=[:cntrl:]]+)=([^\\s()\\[\\]?\\.<>@,;:\\\\/=[:cntrl:]\"]+|\"((?:[^\\\\\"]|\\\\.)*)\")")
  
#|(define (parse-mime-type mt)
  (let* ([parts (string-split mt ";")]
         [type (and (cons? parts) (string->symbol (string-downcase (car parts))))]
         [charset (let loop [(lst (if (cons? parts) (cdr parts) null))]
    

(define (parse-form-data headers body)
  |#

(define CHAR:\" (char->integer #\"))
(define CHAR:\\ (char->integer #\\))

(define (bytes-unescape bs)
  (let ([out (open-output-bytes)]
        [in (open-input-bytes bs)])
    (let loop ([escape? #f])
      (let ([b (read-byte in)])
        (unless (eof-object? b)
          (if escape?
              (cond
                [(or (= b CHAR:\")
                     (= b CHAR:\\))
                 (write-byte b out) (loop #f)]
                [else (write-byte CHAR:\\ out) (write-byte b out) (loop #f)]) 
              (if (= b CHAR:\\) (loop #t) (and (write-byte b out) (loop #f)))))))
    (close-input-port in)
    (get-output-bytes out #f)))
                

(define (parse-mimetype line)
  (let* ([mime-posn (regexp-match-positions mime-regexp line)])
    (if mime-posn
        (let loop ([start (cdar mime-posn)]
                   [acc '()])
          (let ([next-match (regexp-match-positions mime-parameter-regexp line start)])
            (if next-match
                (loop (cdar next-match)
                      (cons 
                       (cons (bytes->string/utf-8 (subbytes line (caadr next-match) (cdadr next-match)))
                             (bytes->string/utf-8
                              (if (cadddr next-match)
                                  (bytes-unescape (subbytes line (car (cadddr next-match)) (cdr (cadddr next-match))))
                                  (subbytes line (caaddr next-match) (cdaddr next-match)))))
                       acc))
                (values (cons (string->symbol (string-downcase (bytes->string/utf-8 (subbytes line (caadr mime-posn) (cdadr mime-posn)))))
                              (string->symbol (string-downcase (bytes->string/utf-8 (subbytes line (caaddr mime-posn) (cdaddr mime-posn))))))
                        acc))))
        (values #f #f))))

(define (parse-form-bindings headers body)
  (let-values
      ([(mime-type mime-parameters) (parse-mimetype (hash-ref headers 'content-type))])
    (let ([encoding (cond [(assoc  "charset" mime-parameters string-ci=?)
                           =>
                           (λ (cs)
                             (string->charset-name (cdr cs)))]
                          [else 'ASCII])])
      (match mime-type
        ['(multipart . form-data)
         (let loop ([lst
                     (filter (λ (x) x)
                             (map parse-part
                                  (regexp-split
                                   (regexp (string-append "--" (regexp-quote (cdr (assoc "boundary" mime-parameters string-ci=?))) "(?:--|\r\n)"))
                                   body)))]
                    [acc '()])
           (if (null? lst) acc
               (let ([ent (message-entity (car lst))])
                 (loop (cdr lst)
                     
                       (if (and (entity-disposition ent) (assoc "name" (disposition-params (entity-disposition ent))))
                           (cons (cons 
                                  (cdr (assoc "name" (disposition-params (entity-disposition ent))))
                                  ent)
                                 acc)
                           acc)))))]
        ['(application . x-www-form-urlencoded)
         (form-urlencoded->alist (bytes->string/utf-8 body))]
        ['(application . json)
         (string->jsexpr (bytes->string/name body encoding))]
        [else
         (bytes->string/name body encoding)]
         ))))
       


(define (parse-part x)
  (and (not (or (bytes=? x #"") (bytes=? x #"--")))
       (mime-analyze x #t)))


(module+ test
  (require racket/bytes)
  (define test-body
    (bytes-join
     '(#"-----------------------------735323031399963166993862150"
       #"Content-Disposition: form-data; name=\"text1\""
       #""
       #"text default"
       #"-----------------------------735323031399963166993862150"
       #"Content-Disposition: form-data; name=\"text2\""
       #""
       #"a\317\211b"
       #"-----------------------------735323031399963166993862150"
       #"Content-Disposition: form-data; name=\"file1\"; filename=\"a.txt\""
       #"Content-Type: text/plain"
       #""
       #"Content of a.txt."
       #""
       #"-----------------------------735323031399963166993862150"
       #"Content-Disposition: form-data; name=\"file2\"; filename=\"a.html\""
       #"Content-Type: text/html"
       #""
       #"<!DOCTYPE html><title>Content of a.html.</title>"
       #""
       #"-----------------------------735323031399963166993862150"
       #"Content-Disposition: form-data; name=\"file3\"; filename=\"binary\""
       #"Content-Type: application/octet-stream"
       #""
       #"a\317\211b"
       #"-----------------------------735323031399963166993862150--"
       #"") #"\r\n"))
    
  (require racket/pretty)
  (define bindings (parse-form-bindings
                    (hasheq 'content-type
                            #"multipart/form-data; boundary=---------------------------735323031399963166993862150") test-body))
  (pretty-print bindings)

  (void (map 
   (λ (pair)
     (match-define (cons name ent) pair)
     (displayln name)
     (printf "MIME: ~a/~a\n" (entity-type ent) (entity-subtype ent))
     (unless (null? (entity-body ent))
       ((entity-body ent) (current-output-port)))
     (displayln "\n-----"))
   bindings))
  
  (parse-form-bindings (hasheq 'content-type #"application/x-www-form-urlencoded")
                       #"field1=value1&field2=value2")
  (parse-form-bindings (hasheq 'content-type #"text/plain; charset=UTF-8")
                       #"a\317\211b")

  )