(** This is the main file if you are using static linking without config file.
 *)

module%shared Myapp = Myapp

let%server _ =
  Ocsigen_server.start
    ~ports:[`All, 8080]
    ~veryverbose:()
    ~debugmode:true
    ~logdir:"local/var/log/myapp"
    ~datadir:"local/var/data/myapp"
    ~uploaddir:(Some "/tmp")
    ~usedefaulthostname:true
    ~command_pipe:"local/var/run/myapp-cmd"
    ~default_charset:(Some "utf-8")
    [ Ocsigen_server.host
      [Staticmod.run ~dir:"local/var/www/myapp" (); Eliom.run ()] ]
