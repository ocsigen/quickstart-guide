open%client Eliom_content.Html
open%shared Eliom_content.Html.D
open%client Js_of_ocaml_lwt
open%client Js_of_ocaml

let%server () = Ocsipersist_settings.set_db_file "local/var/data/myapp/myapp_db"
let%server application_name = "myapp"
let%client application_name = Eliom_client.get_application_name ()

(* Create a module for the application. See
   https://ocsigen.org/eliom/manual/clientserver-applications for more
   information. *)
module%shared App = Eliom_registration.App (struct
    let application_name = application_name
    let global_data_path = Some ["__global_data__"]
  end)

(* As the headers (stylesheets, etc) won't change, we ask Eliom not to
   update the <head> of the page when changing page. (This also avoids
   blinking when changing page in iOS). *)
let%client _ = Eliom_client.persist_document_head ()

let%server main_service =
  Eliom_service.create ~path:(Eliom_service.Path [])
    ~meth:(Eliom_service.Get Eliom_parameter.unit) ()

let%client main_service = ~%main_service

let%server second_service =
  Eliom_service.create ~path:(Eliom_service.Path ["second_page"])
    ~meth:(Eliom_service.Get Eliom_parameter.unit) ()

let%client second_service = ~%second_service

let%server connection_service =
  Eliom_service.create ~path:Eliom_service.No_path
    ~meth:
      (Eliom_service.Post (Eliom_parameter.unit, Eliom_parameter.string "name"))
    ()

let%client connection_service = ~%connection_service

let%server disconnection_service =
  Eliom_service.create ~path:Eliom_service.No_path
    ~meth:(Eliom_service.Post (Eliom_parameter.unit, Eliom_parameter.unit))
    ()

let%client disconnection_service = ~%disconnection_service

let%server username =
  Eliom_reference.eref ~persistent:"username"
    ~scope:Eliom_common.default_session_scope None

let%client username = ref None
let%server get_username () = Eliom_reference.get username
let%client get_username () = Lwt.return !username

let%shared main_page_not_connected () =
  let open Eliom_content.Html.D in
  [ h1 [txt "Welcome to Eliom!"]
  ; Form.post_form ~service:connection_service
      (fun name ->
         [ Form.input ~input_type:`Text ~name Form.string
         ; Form.input ~input_type:`Submit ~value:"Login" Form.string ])
      () ]

let%rpc send_message (msg : string) : unit Lwt.t =
  print_endline msg; Lwt.return ()

let%shared chat () =
  let i = input ~a:[a_input_type `Text] () in
  let b = input ~a:[a_input_type `Submit; a_value "Send"] () in
  ignore
    [%client
      (Lwt.async (fun () ->
         Lwt_js_events.clicks (To_dom.of_element ~%b) (fun _ _ ->
           send_message (Js.to_string (To_dom.of_input ~%i)##.value)))
       : unit)];
  div ~a:[a_class ["chat"]] [i; b]

let%shared main_page_connected name =
  let open Eliom_content.Html.D in
  [ h1 [txt "Welcome "; txt name; txt "!"]
  ; Form.post_form ~service:disconnection_service
      (fun () -> [Form.input ~input_type:`Submit ~value:"Logout" Form.string])
      ()
  ; a ~service:second_service [txt "second page"] ()
  ; chat () ]

(* init has a different implentation on the server and on the client.
   The server version will be called when the page in generated on
   the server, that is only the first page. Subsequent pages are
   generated on the client. *)
let%server init name = ignore [%client (username := Some ~%name : unit)]
let%client init _ = ()

let%shared page name content =
  (match name with None -> () | Some name -> init name);
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

let%shared () =
  App.register ~service:main_service (fun () () ->
    let%lwt name = get_username () in
    Lwt.return
      (page name
         (match name with
         | None -> main_page_not_connected ()
         | Some name -> main_page_connected name)))

let%shared () =
  App.register ~service:second_service (fun () () ->
    let%lwt name = get_username () in
    Lwt.return (page name [a ~service:main_service [txt "home page"] ()]))

(* I'm using a regular html form for this service.
   I don't need a client side version.
   If you want one, the client version could use a RPC
   (as Eliom_reference is a server-side function) *)
let%server () =
  Eliom_registration.Action.register ~service:connection_service (fun () name ->
    ignore [%client (username := Some ~%name : unit)];
    Eliom_reference.set username (Some name))

let%server () =
  Eliom_registration.Action.register ~service:disconnection_service
    (fun () () ->
       ignore [%client (username := None : unit)];
       Eliom_state.discard_all ~scope:Eliom_common.default_session_scope ())
