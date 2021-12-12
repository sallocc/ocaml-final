let show_form ?message request =
  <html>
  <body>

%   begin match message with
%   | None -> ()
%   | Some message ->
      <p>You entered: <b><%s message %>!</b></p>
%   end;

    <%s! Dream.form_tag ~action:"/" request %>
      <button name="Column 1" type="submit"> Go Here!</ button>
    </form>

  </body>
  </html>

let () =
  Dream.run
  @@ Dream.logger
  @@ Dream.memory_sessions
  @@ Dream.router [

    Dream.get  "/"
      (fun request ->
        Dream.html (show_form request));

    Dream.post "/"
      (fun request ->
        match%lwt Dream.form request with
        | `Ok ["message", message] ->
          Dream.html (show_form ~message request)
        | _ ->
          Dream.empty `Bad_Request);

  ]
  @@ Dream.not_found