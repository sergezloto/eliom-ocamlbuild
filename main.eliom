{shared{
  open Eliom_lib
  open Html
  open App

  let value = Dep.a_value
}}

{client{
  let () = alert "Once only during initialization of the client, i.e. before the document is available."
  let () =
    Eliom_client.onload
      (fun () -> alert "Once only when the document is put in place.")
}}
{server{
  let _ = App.register_service ~path:[] ~get_params:Eliom_parameter.unit
    (fun () () ->
      ignore {unit{
        alert "Each time this service is called and the sent document is put in place."
      }};
      Lwt.return
        (html
          (head (title (pcdata "Hi!")) [])
          (body [p [pcdata "Built with ocamlbuild."]]))
    )
}}
