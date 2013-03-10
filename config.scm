;; IRC configuration

(define *net-configuration*
  '([(name     . "tenthbit")
     (server   . "irc.tenthbit.net")
     (nick     . "beepbopboop")
     (channels . ("#spam" "#spam2"))
     (login   . ([user . "tenthbit_boop"]
                 [pass . "beep"]))]

    [(name     . "tenthbit2")
     (server   . "irc.tenthbit.net")
     (nick     . "beepbopboop2")
     (channels . ("#spam"))
     (login   . ([user . "tenthbit_boop2"]
                 [pass . "beep2"]))]))

(define *server-configuration*
  '([port . 5555]))
