#lang scribble/manual
@require[@for-label[net/scgi
                    @except-in[racket/base date? date]
                    net/cookie net/mime racket/tcp racket/unix-socket gregor @except-in[xml entity? entity struct:entity make-entity] json]]

@title{net-scgi}
@author{djholtby}

@defmodule[net/scgi]

Package Description Here



@section{Procedure Contracts}

@defthing[scgi-responder/c contract? #:value (-> scgi-request? input-port? output-port? any/c)]{An SCGI responder is passed an
                                                                                                @racket[scgi-request] structure as well as
                                                                                                the input and output ports for the SCGI
                                                                                                connection.

                                                                                                You should only write your own responder if
                                                                                                you need to write something other than an
                                                                                                HTTP response to the output port.  For typical
                                                                                                usage use @racket[make-scgi-responder].

                                                                                                The return value is ignored by @racket[scgi-serve]
}

                                                                                               
                                                                                                
@defthing[scgi-handler/c contract? #:value (-> scgi-request? any/c)]{An SCGI handler is passed a SCGI request and returns a response.

                                                                     A response that is not an @racket[scgi-response] is coerced into a string
                                                                     and written with status 200 and content-type: text/plain}
                                                                     
                                                                     
@defthing[scgi-websock-handler/c contract? #:value (-> scgi-ws-conn? any/c any/c)]{An SCGI websocket handler will be passed an @racket{scgi-ws-conn}
on a successful websocket handshake. The second parameter is a context value, by default the @racket[scgi-request?] for the
 initial connection.  See @racket{make-websock-responder}                                                                                    
                                                                                                                               }

@section{The Server}

@defproc[(scgi-serve [responder scgi-responder/c]
                     [#:port port port-number? 4000]
                     [#:max-allow-wait max-allow-wait exact-positive-integer? 4]
                     [#:reuse? reuse? any/c #f]
                     [#:hostname hostname (or/c string? #f) "127.0.0.1"])
                       void?]{Starts a SCGI server on the given hostname and port.

 On each SCGI request, the responder will be passed the @racket[scgi-request] as well as the input and output ports for the connection.
 Each request is handled in its own thread.

 The optional keyword arguments @racket[#:hostname], @racket[#:max-allow-wait], and @racket[#:reuse?] are passed to
 @racket[tcp-listen].
}

@defproc[(scgi-serve/socket
          [responder scgi-responder/c]
          [path unix-socket-path?]
          [#:max-allow-wait max-allow-wait exact-positive-integer? 4]) void?]{Like @racket[scgi-serve] but instead of listening on a TCP port,
 listens on a Unix socket.  @racket[#:max-allow-wait] corresponds to the @racket[backlog] parameter of @racket[unix-socket-listen]}
                                                                        
@section{Responders}

@defproc[(make-scgi-responder [handler scgi-handler/c]) scgi-responder/c]{Creates a simple @racket[scgi-responder/c] procedure that
                                                                                           passes the @racket[scgi-request] to @racket[handler]
                                                                                           and then writes the resulting @racket[scgi-response]
                                                                                           to the output port.}
@defproc[(make-websock-responder [handler scgi-websock-handler/c] [#:on-connect on-connect (or/c #f (-> scgi-request? (values ((listof bytes?) any/c)))) #f])
        scgi-responder/c]{Creates a @racket[sgci-responder/c] procedure that performs a websocket handshake, and on success
                                    creates a new @racket[scgi-websock-conn] connection and passes this to @racket[handler].

                                    As @racket[scgi-serve] handles each new connection in its own thread it is safe for the handler to loop
                                    until the socket is closed.

                                    If @racket[on-connect] is not @racket[#f] then it is called prior to completng the handshake.
                                    Its first return value is a list of any additonal headers to add to the handshake response
                                    (e.g. this can be useful for setting a cookie or JWT).  Its second return value is the state value that will
                                    be passed as the second parameter to @racket[handler].

                                    If @racket[on-connect] is @racket[#f] then the second parameter to @racket[handler] will be the initial
                                    @racket[scgi-request].

}

@section{Request and Response}

The @racket{net/scgi} module uses @racket{scgi-request} and @racket{scgi-response} structs to encapsulate an SCGI request and a
standard HTTP response, respectively.  This is similar to the Racket webserver.  

@defstruct*[scgi-request ([method string?]
                         [scheme (or/c "HTTP" "HTTPS")]
                         [host string?]
                         [uri url?]
                         [cookies (promise/c (listof (cons/c bytes? bytes?)))]
                         [post-data (promise/c (or/c (listof (pair/c symbol? string?))
                                                     (listof (pair/c symbol? entity?))
                                                     #f))]
                         [content (pair/c (or/c #f symbol?) (promise/c bytes?))]
                         [headers (listof bytes?)]) #:transparent]{Represents an SCGI request.  Method, scheme, host, uri come from the
                                                                   SCGI headers.

                                                                   Cookies is a promise that when forced will extract cookie headers and
                                                                   parse them into an association list.  This allows you to avoid
                                                                   unnecessary processing if cookies are not needed.

                                                                   Post data is a promise that when forced will extract values from a POST
                                                                   (or similar method) request.

                                                                   For application/x-www-form-urlencoded this will be an association list
                                                                   where the keys are symbols and the values are strings.

                                                                   For multipart/form-data this will be an association list where the keys
                                                                   are symbols and the values are @racket[entity] values.

                                                                   For all other types, the value will be #f (and you can deal with content manually)
                                                                   }

@defstruct*[scgi-response ([status status-code?]
                           [date moment?]
                           [content-type string?]
                           [cookies (listof cookie?)]
                           [other-headers (listof bytes?)]
                           [content (or/c string? bytes?)])
            #:transparent]{Represents a standard HTTP response.  Should generally be created with one of the following constructor functions
                           rather than directly initializing the fields}

@defproc[(make-response [content-type string?]
                        [content (or/c string? bytes?)]
                        [#:status status status-code? 200]
                        [#:date date moment? (now/moment/utc)]
                        [#:cookies cookies (listof cookie?) '()]
                        [#:headers headers (listof bytes?) '()])
          scgi-response?]{Constructs an HTTP response.  Defaults to the current time and status code 200, though these can be overriden using
                          the @racket[#:date] and @racket[#:status] keyword arguments.

                          @racket[#:cookies] can be used to set cookies if desired, and @racket[#:headers] can be used to add other extra
                          headers.}

@defproc[(make-response/text [content (or/c string? bytes?)]
                             [#:content-type content-type string? "text/plain"]
                             [#:status status status-code? 200]
                             [#:date date moment? (now/moment/utc)]
                             [#:cookies cookies (listof cookie?) '()]
                             [#:headers headers (listof bytes?) '()])
         scgi-response?]{Shorthand for @racket[make-response] that automatically selects the text/plain MIME-type (though this can be overridden)}

@defproc[(make-response/xexpr [content xexpr?]
                             [#:content-type content-type string? "text/html"]
                             [#:status status status-code? 200]
                             [#:date date moment? (now/moment/utc)]
                             [#:cookies cookies (listof cookie?) '()]
                             [#:headers headers (listof bytes?) '()])
         scgi-response?]{Shorthand for @racket[make-response] that converts content from an X-Expression to a string, and selects the
                                       text/html MIME-type (which can be overridden with @racket[#:content-type])}

                          
@defproc[(make-response/jsexpr [content jsexpr?]
                             [#:content-type content-type string? "application/json"]
                             [#:status status status-code? 200]
                             [#:date date moment? (now/moment/utc)]
                             [#:cookies cookies (listof cookie?) '()]
                             [#:headers headers (listof bytes?) '()])
         scgi-response?]{Shorthand for @racket[make-response] that converts content from a JSON expression to a string, and selects the
                                       application/json MIME-type (which can be overridden with @racket[#:content-type])}


@section{Websockets}

If you are using nginx as your webserver, it leaves the SCGI sockets open, so it is possible to implement websockets.  To do so, use @racket[make-websock-responder] to create an appropriate responder.

@defstruct[scgi-ws-conn ([connected? any/c #:mutable]) #:omit-constructor]{An opaque struct used to handle a websocket connection.  Communicate using @racket[scgi-websocket-send!] and @racket[scgi-websocket-read!].
                                                                                                                                                      The connection may be closed using @racket[scgi-websocket-close!].

                                                                                                                                                      New instances can only be created by the websocket responder procedure.  @racket[scgi-ws-conn-connected?] is the only selector method exported.

                                                                                                                                                      The struct is synchronizable, and the synchronization is ready when the input socket has unread data (however, this data may be a control frame so
                                                                                                                                                      it is not a garauntee that @racket[scgi-websocket-read!] will not block.}

@defproc[(scgi-websocket-send! [conn scgi-ws-conn?]
                               [message (or/c string? bytes?)])
         void?]{If conn is connected, send message.  String messages are sent as UTF-8 in a text frame.  Bytes messages are sent as-is in a binary frame.  Frames are not fragmented.}

@defproc[(scgi-websocket-read! [conn scgi-ws-conn?]) (or/c string? bytes? eof-object?)]{Blocks until a data frame is received.  Text frames are converted to strings by @racket[bytes->string/utf-8].  Binary frames are kept as byte strings.  If the connection is closed, returns @racket[eof].
                                                                                                                                                                        Accepts fragmented frames.}

@defproc[(scgi-websocket-close! [conn scgi-ws-conn] [#:reason reason bytes? #""]) void?]{Closes @racket[conn] if it was not already closed.  If present, reason is sent as the body of the disconnect frame.}
                                                                                                                                                                 
                                                                                 

}
