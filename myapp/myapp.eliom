open Eliom_content.Html.D

let () = Ocsipersist_settings.set_db_file "local/var/data/myapp/myapp_db"

let main_service =
  Eliom_service.create ~path:(Eliom_service.Path [])
    ~meth:(Eliom_service.Get Eliom_parameter.unit) ()

let second_service =
  Eliom_service.create ~path:(Eliom_service.Path ["second_page"])
    ~meth:(Eliom_service.Get Eliom_parameter.unit) ()

let connection_service =
  Eliom_service.create ~path:Eliom_service.No_path
    ~meth:
      (Eliom_service.Post (Eliom_parameter.unit, Eliom_parameter.string "name"))
    ()

let disconnection_service =
  Eliom_service.create ~path:Eliom_service.No_path
    ~meth:(Eliom_service.Post (Eliom_parameter.unit, Eliom_parameter.unit))
    ()

let username =
  Eliom_reference.eref ~persistent:"username"
    ~scope:Eliom_common.default_session_scope None

let main_page_not_connected () =
  [ h1 [txt "Welcome to Eliom!"]
  ; (let open Eliom_content.Html.F in
     Form.post_form ~service:connection_service
       (fun name ->
          [ Form.input ~input_type:`Text ~name Form.string
          ; Form.input ~input_type:`Submit ~value:"Login" Form.string ])
       ()) ]

let main_page_connected name =
  [ h1 [txt "Welcome "; txt name; txt "!"]
  ; Form.post_form ~service:disconnection_service
      (fun () -> [Form.input ~input_type:`Submit ~value:"Logout" Form.string])
      ()
  ; a ~service:second_service [txt "second page"] () ]

let page content =
  html
    (head
       (title (txt "myapp"))
       [ css_link
           ~uri:
             (make_uri
                ~service:(Eliom_service.static_dir ())
                ["css"; "myapp.css"])
           () ])
    (body content)

let () =
  Eliom_registration.Html.register ~service:main_service (fun () () ->
    let%lwt name = Eliom_reference.get username in
    Lwt.return
      (page
         (match name with
         | None -> main_page_not_connected ()
         | Some name -> main_page_connected name)))

let () =
  Eliom_registration.Html.register ~service:second_service (fun () () ->
    Lwt.return (page [a ~service:main_service [txt "home page"] ()]))

let () =
  Eliom_registration.Action.register ~service:connection_service (fun () name ->
    Eliom_reference.set username (Some name))

let () =
  Eliom_registration.Action.register ~service:disconnection_service
    (fun () () ->
       Eliom_state.discard_all ~scope:Eliom_common.default_session_scope ())
