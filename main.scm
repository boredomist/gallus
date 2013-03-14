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

(define-constant BUFFER-SIZE 500)

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
      (map-add! tm target (channel target))
      (irc-connection-channels-set! conn (persist-map! tm))))

  (map-ref (irc-connection-channels conn) target))

(define (join-channel! connection target)
  (irc:join (irc-connection-conn connection) target)
  ;; TODO: add self to users list
  (add-scrollback-target! connection target))

(define (add-client! connection in out)
  (let ([clients (irc-connection-clients connection)])
    (irc-connection-clients-set! connection
                                 ;; Well this can't be right...
                                 (append clients (list (cons in out))))))

;; Send a message to each connected client
(define (dispatch-string connection str)
  (let ([clients (irc-connection-clients connection)])
    (for-each
     (lambda (client)
       (let ([in  (car client)]
             [out (cdr client)])
         (if (any port-closed? (list in out))
             ;; TODO: remove closed ports from list
             '()
             (write-line str out))))
     clients)))

(define (dispatch-message connection msg)
  (dispatch-string connection (format "~A" (irc:message-body msg))))

(define-record channel
  name
  buffer
  users
  topic
  modes)

(define (channel name)
  (make-channel
   name                                             ; Channel name
   (apply circular-list (make-list BUFFER-SIZE #f)) ; Scrollback
   '()                                              ; Users
   "No topic is set"                                ; Topic
   ""                                               ; Channel modes
   ))

(define (channel-push! chan msg)
  (let ([buf (channel-buffer chan)])
    (set-car! buf msg)
    (channel-buffer-set! chan (cdr buf))))

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
                                       ;; TODO: channel modes, users, topic, etc.
                                       (for-each (cut join-channel! connection <>)
                                                 channels))
                                code: 001)

      ;; Store PRIVMSGs to channel buffers
      (irc:add-message-handler!
       conn
       (lambda (msg)
         (let* ([target (irc:message-receiver msg)]
                [channel (add-scrollback-target! connection target)])
           (write-line (format "pushing ~a to ~a" (irc:message-body msg) target))
           (channel-push! channel (cons (irc:message-body msg)
                                        (seconds->string
                                         (irc:message-timestamp msg))))))
       command: "PRIVMSG")

      ;; Store users currently on channel
      (irc:add-message-handler!
       conn
       (lambda (msg)
         (let ([channels (irc-connection-channels connection)]
               [match (irregex-match (irregex ".*? (#.*?) :(.*)")
                                     (irc:message-body msg))])
           (unless (null? match)
             (let* ([target (irregex-match-substring match 1)]
                    [users  (irregex-match-substring match 2)]
                    [channel (map-ref channels target)])
               (when channel
                 (channel-users-set! channel (string-split users " ")))))))
       code: 353)

      ;; TODO: JOIN/PART handling is pretty messy, need to rewrite eventually
      ;; When we get a PART, update the users list
      (irc:add-message-handler!
       conn
       (lambda (msg)
         (let* ([nick (irc:message-sender msg)]
                [target (irc:message-receiver msg)]
                [channel (map-ref (irc-connection-channels connection) target)])

           (when channel
             (channel-users-set! channel (filter (lambda (user)
                                                   (any (cut equal? nick <>)
                                                        (map (lambda (prefix)
                                                               (format "~a~a" prefix user))
                                                             '("" "+" "@"))))
                                          (channel-users channel))))))
       command: "PART")


      ;; Ditto for JOINs
      ;; FIXME: Self not added to channel list
      (irc:add-message-handler!
       conn
       (lambda (msg)
         (let* ([nick (irc:message-sender msg)]
                [target (irc:message-receiver msg)]
                [channel (map-ref (irc-connection-channels connection) target)])
           (when
               (and channel
                    (not (any
                          (lambda (user)
                            (any (cut equal? nick <>)
                                 (list user (string-append "+" user)
                                       (string-append "+" user))))

                          (channel-users channel))))

             (channel-users-set! channel
                                 (append (channel-users channel) (list nick))))))

       command: "JOIN")

      ;; Channel topic
      (irc:add-message-handler!
       conn
       (lambda (msg)
         (let ([channels (irc-connection-channels connection)]
               [match (irregex-match (irregex ".*? (#.*?) :(.*)")
                                     (irc:message-body msg))])
           (unless (null? match)
             (let* ([target (irregex-match-substring match 1)]
                    [topic  (irregex-match-substring match 2)]
                    [channel (map-ref channels target)])
               (when channel
                 (channel-topic-set! channel topic))))))
       code: 332)

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
               (dispatch-string connection msg)
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

    (send-as-server (format "001 ~a :- Welcome to Gallus" nick) out)

    (map-each (lambda (target channel)
                ;; Avoid JOINing conversations with other people
                (when (string-prefix? "#" target)

                  ;; TODO: Better (actual) mask parsing
                  (write-line (format ":~a!your@mask.todo JOIN :~a" nick target)
                              out)

                  ;; Send out NAMES
                  (send-as-server
                   (format "353 ~a @ ~a :~a" nick target
                           (string-join (channel-users channel) " "))
                   out)

                  (send-as-server (format "366 ~a: End of /NAMES list"
                                          target)
                                  out)

                  ;; Set the topic
                  (send-as-server (format "332 ~a ~a :~a" nick target
                                          (channel-topic channel))
                                  out))

                ;; Replay scrollback
                (let ([scrollback (drop-while (cut equal? #f <>)
                                              (take (channel-buffer channel)
                                                    BUFFER-SIZE))])
                  (for-each
                   (lambda (line)
                     (let ([msg (car line)]
                           [time (cdr line)])
                       (write-line (format "~a [~a]" msg time)
                                   out)))
                   scrollback)))

              targets)

    (add-client! conn in out)

    (let loop ()
      (let* ([line (read-line in)]
             [prefix? (lambda (str) (string-prefix? str line))])
        (if (and (irc:connected? irc)
                 (not (eof-object? line)))
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
            [match (if (eof-object? line)
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

       (unless (eof-object? line)
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

(let ([server-thread (thread-start! start-server)]
      [clients-thread (thread-start! connect-to-networks)])

  (thread-join! clients-thread))
