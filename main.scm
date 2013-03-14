(use extras irc irregex posix persistent-hash-map srfi-1 srfi-2 srfi-18 srfi-26 tcp)

(load "config.scm")

(define *irc-connections* (persistent-map))
(define *irc-threads* '())
(define *channel-buffer* #f)

(define-constant DEFAULT-NET-CONFIG
  '([nick . "iforgottosetanick"]
    [port . 6667]
    [user . "schemethingy"]
    [channels . ()]
    [real-name . "schemethingy"]))

(define-constant DEFAULT-SERVER-CONFIG
  '([port . 5555]
    [timeout . 15]
    [hostname . "gallus.scm"]))

;; Send something as the IRC server
(define (send-as-server msg out)
  (write-line (format ":~a ~a" (server-conf 'hostname) msg) out))

(define (server-conf key)
  (cdr (or (assoc key *server-configuration*)
           (assoc key DEFAULT-SERVER-CONFIG))))

;; PASS <USER>:<PASSWORD>, for authenticating client connections
(define PASS-REGEXP (irregex "^pass (.*?):(.*)$" 'i))

(define *handlers* '())

(define-record irc-connection conn config channels clients)

(define (irc-connection conn config)
  (make-irc-connection conn config (persistent-map) '()))

(define (add-scrollback-target! conn target)
  (unless (map-contains? (irc-connection-channels conn) target)
    ;; I feel dirty.
    (let ([tm (map->transient-map (irc-connection-channels conn))])
      (map-add! tm target (channel-buffer 500))
      (irc-connection-channels-set! conn (persist-map! tm)))))

(define (join-channel! connection target)
  (irc:join (irc-connection-conn connection) target)
  (add-scrollback-target! connection target))

(define (add-client! connection in out)
  (let ([clients (irc-connection-clients connection)])
    (irc-connection-clients-set! connection
                                 ;; Well this can't be right...
                                 (append clients (list (list in out))))))

;; Send a message to each connected client
(define (dispatch-message connection msg)
  (let ([clients (irc-connection-clients connection)])
    (for-each
     (lambda (client)
       (let ([out (cadr client)])
         (write-line (format "~A" (irc:message-body msg)) out)))
     clients)))

(define-record channel-buffer
  size
  buffer)

(define (channel-buffer size)
  (make-channel-buffer
   size
   (apply circular-list (make-list size #f))))

(define (channel-buffer-append buf msg)
  (let ([buf (channel-buffer-buffer chanbuf)])
    (set-car! buf msg)
    (set! buf (cdr buf))))

(define (make-connection config)
  (define ($ key . optional?)
    "Helper to get configuration values"
    (let ((value (assoc key config)))
      (if value
          (cdr value)
          (if (null? optional?)
              (error (sprintf "required value for ~A" key))
              (cdr (assoc key DEFAULT-NET-CONFIG))))))

  (let ((name      ($ 'name))
        (server    ($ 'server))
        (port      ($ 'port #t))
        (nick      ($ 'nick))
        (real-name ($ 'real-name #t))
        (user      ($ 'user #t))
        (channels  ($ 'channels #t)))

    (define (log fmt #!rest args)
      (printf "~a\t:: ~A~n" name (format fmt args)))

    (printf "Connecting to ~a (~A:~A)~n" name server port)

    (let* ([conn (irc:connection server: server
                                 nick: nick
                                 real-name: real-name
                                 port: port
                                 user: user)]
           [connection (irc-connection conn config)])

      (for-each
       (lambda (handler)
         (define ($ key)
           (let ([val (assoc key handler)])
             (if val (cdr val) "")))

         (let ([command ($ 'command)]
               [body    ($ 'body)]
               [tag     ($ 'tag)]
               [func    ($ 'func)])
           (irc:add-message-handler! conn (lambda (msg) ((eval func) conn msg))
                                     command: command body: body
                                     tag: tag)))

       *handlers*)

      (irc:add-message-handler!
       conn (lambda (msg)
              (irc:command conn (string-append
                                 "PONG :" (car (irc:message-parameters msg)))))
       tag: 'ping
       command: "PING")

      ;; Join some channels when we get 001
      (irc:add-message-handler! conn (lambda (_)
                                       (for-each (cut join-channel! connection <>)
                                                 channels))
                                code: 001)

      (irc:connect conn)

      (let ([tm (map->transient-map *irc-connections*)])
        (map-add! tm name connection)
        (set! *irc-connections* (persist-map! tm)))

      (let loop ()
        (condition-case

         (let message-loop ()
           (let ([msg (irc:wait conn)])
             (irc:process-message conn msg)

             (dispatch-message connection msg)

             (log "~a" (irc:message-body msg)))
           (message-loop))

         (ex (i/o net)
             (irc:disconnect conn)
             (print-error-message ex (current-error-port))
             (log "Disconnected, waiting ...")
             (thread-sleep! 40)
             (log "Reconnecting...")
             (irc:connect conn)
             (loop))

         (ex (irc/eof)
             (irc:disconnect conn)
             (log "Received eof from server...")
             (thread-sleep! 40)
             (log "Reconnecting...")
             (irc:connect con)
             (loop))

         (ex (irc)
             (let ([code (get-condition-property ex 'irc 'code)]
                   [msg (get-condition-property ex 'exn 'message)])
               (printf "Caught exception ~a: ~a~n" code msg)
               (case code

                 ;; TODO: Do actual uniquifying of nick instead of just
                 ;; defaulting to this.
                 [(433) (irc:command conn "NICK my_nick_was_stolen")]
                 [(421) (printf "Seems we used an unknown command")]
                 [else  (printf "Don't know how to handle ~a, assuming we're still afloat~n"
                                code)]))

             (loop)))))))

(define (connect-to-networks)
  (set! *irc-threads*
        (map (lambda (conf) (make-thread
                             (lambda () (make-connection conf))))
             *net-configuration*))

  (for-each thread-start! *irc-threads*)
  (for-each thread-join! *irc-threads*))

(define (begin-client-loop config in out)
  (let* ([name (cdr (assoc 'name config))]
         [conn (map-ref *irc-connections* name)]
         [irc (irc-connection-conn conn)]
         [targets (irc-connection-channels conn)]
         [nick (irc:connection-nick irc)])

    (send-as-server (format "001 ~a :- Welcome to Gallus"
                            nick) out)

    (add-client! conn in out)

    (map-each (lambda (target buffer)
                ;; TODO: Better (actual) mask parsing
                (write-line (format ":~a!your@mask.todo JOIN :~a" nick target)
                            out)
                ;; TODO: Store this instead of querying each time.
                (when (irc:connected? irc)
                  (irc:command irc (format "NAMES ~a" target))))

              targets)

    (let loop ()
      (let* ([line (read-line in)]
             [prefix? (lambda (str) (string-prefix? str line))])
        (if (and (irc:connected? irc)
                 (not (equal? line #!eof)))
            ;; We may want to gobble the line for our own greedy purposes
            (unless (or (prefix? "USER")
                        (prefix? "CAP")
                        (prefix? "QUIT")
                        (prefix? "PONG"))
              (irc:command irc line))

            (send-as-server "ERROR :You're not connected to a server!" out)))
      (loop))))

(define (handle-client in out return)
  (write-line "identify yourself" out)
  (condition-case
   (let loop ()
     (let* ([timeout (server-conf 'timeout)]
            [line (read-line in)]
            [match (if (equal? line #!eof)
                       #f
                       (irregex-match PASS-REGEXP line))])

       (printf "~a~n" line)

       (if match
           (let ([try-user (irregex-match-substring match 1)]
                 [try-pass (irregex-match-substring match 2)])
             (for-each
              (lambda (conf)
                (let* ([login (cdr (assoc 'login conf))]
                       [user   (cdr (assoc 'user login))]
                       [pass   (cdr (assoc 'pass login))])
                  (when (and (equal? user try-user)
                             (equal? pass try-pass))
                    (return conf))))

              *net-configuration*)

             (write-line (format "Nope, try again in ~a seconds"
                                 timeout) out)
             ;; Stupid simple rate limiting. Can be abused by opening
             ;; a new connection.
             (thread-sleep! timeout))
           (write-line "Hey, you need to login first" out))

       (unless (equal? line #!eof)
         (loop))))

   (ex (i/o net)
       (close-input-port in)
       (close-output-port out)
       (display "Client timed out\n"))))

(define (start-server)
  (let* ([port (server-conf 'port)]
         [sock (tcp-listen port)])
    (tcp-read-timeout #f)

    (display "Spinning up server...\n")
    (let loop ()
      (let-values (((in out) (tcp-accept sock)))
        (thread-start!
         (lambda ()
           (let ([return (lambda (conf)
                           (write-line "Hey, thanks for authenticating, guy" out)
                           (begin-client-loop conf in out))])
             (handle-client in out return)))))
      (loop))

    (display "Shutting down server...\n")
    (tcp-close sock)))

(let ([server-thread (thread-start! (lambda ()  (start-server)))]
      [clients-thread (thread-start! (lambda () (connect-to-networks)))])

  (thread-join! clients-thread))
