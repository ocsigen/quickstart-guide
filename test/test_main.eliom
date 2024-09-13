(** This is the main file if you are using static linking without config file.
 *)

module%shared Test = Test

let%server _ =
  Ocsigen_server.start
    ~ports:[`All, 8080]
    ~veryverbose:()
    ~debugmode:true
    ~logdir:"local/var/log/test"
    ~datadir:"local/var/data/test"
    ~uploaddir:(Some "/tmp")
    ~usedefaulthostname:true
    ~command_pipe:"local/var/run/test-cmd"
    ~default_charset:(Some "utf-8")
    [ Ocsigen_server.host
      [Staticmod.run ~dir:"local/var/www/test" (); Eliom.run ()] ]
