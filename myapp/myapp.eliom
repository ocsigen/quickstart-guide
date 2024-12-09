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
    ~scope:Eliom_common.default_session_scope
    (None : string option)

let%client username = ref None
let%server get_username () = Eliom_reference.get username
let%client get_username () = Lwt.return !username

module%server Notif = Eliom_notif.Make_Simple (struct
    type identity = string option (* current user (sender name) *)
    type key = string (* chat id (recipient name) *)
    type notification = string * string (* sender, message *)

    let get_identity = get_username
  end)

let%shared main_page_not_connected () =
  let open Eliom_content.Html.D in
  [ h1 [txt "Welcome to Eliom!"]
  ; Form.post_form ~service:connection_service
      (fun name ->
         [ Form.input ~input_type:`Text ~name Form.string
         ; Form.input ~input_type:`Submit ~value:"Login" Form.string ])
      () ]

let%rpc send_message (recipient : string) (msg : string) : unit Lwt.t =
  let%lwt me =
    match%lwt Eliom_reference.get username with
    | None -> Lwt.return "<someone>"
    | Some n -> Lwt.return n
  in
  Notif.notify recipient (me, msg);
  Lwt.return ()

let%shared chat () =
  let user = input ~a:[a_input_type `Text] () in
  let msg = input ~a:[a_input_type `Text] () in
  let b = input ~a:[a_input_type `Submit; a_value "Send"] () in
  ignore
    [%client
      (Lwt.async (fun () ->
         Lwt_js_events.clicks (To_dom.of_element ~%b) (fun _ _ ->
           send_message
             (Js.to_string (To_dom.of_input ~%user)##.value)
             (Js.to_string (To_dom.of_input ~%msg)##.value)))
       : unit)];
  div ~a:[a_class ["chat"]] [user; msg; b]

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
let%server init (name : string) =
  ignore [%client (username := Some ~%name : unit)];
  let%lwt () = Notif.init () in
  let e : (string * (string * string)) Eliom_react.Down.t =
    Notif.client_ev ()
  in
  Notif.listen name;
  ignore
    [%client
      (Eliom_lib.Dom_reference.retain Js_of_ocaml.Dom_html.window
         ~keep:
           (React.E.map
              (fun (_, (name, msg)) ->
                 Manip.appendToBody
                   (div ~a:[a_class ["message"]] [txt name; txt ": "; txt msg]))
              ~%e)
       : unit)];
  Lwt.return ()

(* To avoid memory leaks, effectful react events are garbage collected as soon as possible.
   To prevent this, you need to attach them to some page element (here Dom_html.window)
   using function retain. *)


let%client init _ = Lwt.return ()

let%shared page name content =
  let%lwt () =
    match name with None -> Lwt.return () | Some name -> init name
  in
  Lwt.return
    (html
       (head
          (title (txt "myapp"))
          [ css_link
              ~uri:
                (make_uri
                   ~service:(Eliom_service.static_dir ())
                   ["css"; "myapp.css"])
              () ])
       (body content))

let%shared () =
  App.register ~service:main_service (fun () () ->
    let%lwt name = get_username () in
    page name
      (match name with
      | None -> main_page_not_connected ()
      | Some name -> main_page_connected name))

let%shared () =
  App.register ~service:second_service (fun () () ->
    let%lwt name = get_username () in
    page name [a ~service:main_service [txt "home page"] ()])

(* I'm using a regular html form for this service.
   I don't need a client side version.
   If you want one, the client version could use a RPC
   (as Eliom_reference is a server-side function) *)
let%server () =
  Eliom_registration.Action.register ~service:connection_service
    (fun () (name : string) -> Eliom_reference.set username (Some name))

let%server () =
  Eliom_registration.Action.register ~service:disconnection_service
    (fun () () ->
       let%lwt () =
         match%lwt Eliom_reference.get username with
         | None -> Lwt.return ()
         | Some name -> Notif.unlisten name; Lwt.return ()
       in
       ignore [%client (username := None : unit)];
       Eliom_state.discard_all ~scope:Eliom_common.default_session_scope ())
