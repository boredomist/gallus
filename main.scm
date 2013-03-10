(use extras irc irregex posix persistent-hash-map srfi-1 srfi-2 srfi-18 srfi-26 tcp)

(define *irc-connections* '())

(define *channel-buffer* #f)

(define-constant DEFAULT-CONFIG '([nick . "iforgottosetanick"]
                         [port . 6667]
                         [user . "schemethingy"]
                         [channels . ()]
                         [real-name . "schemethingy"]))

(define-constant SERVER-PORT 5542)

(define *irc-configuration*
  '(
    [(name     . "tenthbit")
     (server   . "irc.tenthbit.net")
     (nick     . "beepbopboop")
     (channels . ("#spam" "#spam2"))]

    [(name     . "tenthbit2")
     (server   . "irc.tenthbit.net")
     (nick     . "beepbopboop2")
     (channels . ("#spam"))]))

(define *handlers*
  '(
    [(command  . "PRIVMSG")
     (body     . "asdf")
     (func     . (lambda (conn msg)
                   (irc:say conn "haha, you said butts"
                            (irc:message-receiver msg))))]))

(define-record irc-connection conn config channels)

(define (irc-connection conn config)
  (make-irc-connection conn config (map->transient-map (persistent-map))))

(define (add-scrollback-target conn target)
  (unless (map-contains? (irc-connection-channels conn) target)
    (map-add! (irc-connection-channels conn) target (channel-buffer 500))))

(define (join-channel connection target)
  (irc:join (irc-connection-conn connection) target)
  (add-scrollback-target connection target))

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
  (define ($ key . optional?) "Helper to get configuration values"
    (let ((value (assoc key config)))
      (if value
          (cdr value)
          (if (null? optional?)
              (error (sprintf "required value for ~A" key))
              (cdr (assoc key DEFAULT-CONFIG))))))

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
                                       (for-each (cut join-channel connection <>)
                                                 channels))
                                code: 001)

      (irc:connect conn)

      (let loop ()
        (condition-case

         (let message-loop ()
           (let ([msg (irc:wait conn)])
             (irc:process-message conn msg)
             (log "~a" (irc:message-body msg)))
           (message-loop))

         (ex (i/o net)
             (irc:disconnect conn)
             (print-error-message ex (current-error-port))
             (log "Disconnected, waiting ...")
             (thread-sleep! 20)
             (log "Reconnecting...")
             (irc:connect conn)
             (loop)))))))

(define (connect-networks)
  (set! *irc-connections* (map (lambda (conf) (make-thread (lambda () (make-connection conf)))) *irc-configuration*))
  (for-each thread-start! *irc-connections*)
  (for-each thread-join! *irc-connections*))


(define (handle-client in out)
  (define PASS-REGEXP (irregex "^pass (.*?):(.*)$" 'i))

  (write-line "identify yourself" out)
  (condition-case
   (let loop ()
     (let* ([line (read-line in)]
            [match (irregex-match PASS-REGEXP line)])

       (printf "~a~n" line)

       (if match
           (let ([user (irregex-match-substring match 1)]
                 [pass (irregex-match-substring match 2)])
             (printf "Trying to log in with ~a, ~a ~n" user pass))
           (write-line "Hey, you need to login first" out))

       (unless (equal? line #!eof )
         (loop))))
   (ex (i/o net timeout)
       (close-input-port in)
       (close-output-port out)
       (display "Client timed out\n"))))

(define (start-server)
  (let ([sock (tcp-listen SERVER-PORT)])
    (tcp-read-timeout #f)

    (display "Spinning up server...\n")
    (let loop ()
        (let-values (((in out) (tcp-accept sock)))
          (thread-start! (lambda () (handle-client in out))))
      (loop))

    (display "Shutting down server...\n")
    (tcp-close sock)))


(let ([server-thread (thread-start! (lambda ()  (start-server)))]
      [clients-thread (thread-start! (lambda () (connect-networks)))])

  (thread-join! clients-thread))
