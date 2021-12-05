let show_form ?message request =
  <html>
  <body>

%   begin match message with
%   | None -> ()
%   | Some message ->
      <p>You entered: <b><%s message %>!</b></p>
%   end;

    <%s! Dream.form_tag ~action:"/" request %>
      <button name="col1" type=submit style="width:75px; height:75px;">
        <img src="https://toppng.com/uploads/preview/red-circle-1155276042606ekqvli9k.png"
        style="width:50px; height:50px;">
      </button>
      <button name="col2" type=submit style="width:75px; height:75px; margin:0px">
        <img src="https://toppng.com/uploads/preview/red-circle-1155276042606ekqvli9k.png"
        style="width:50px; height:50px;">
      </button>
      <button name="col3" type=submit style="width:75px; height:75px; margin:0px">
        <img src="https://toppng.com/uploads/preview/red-circle-1155276042606ekqvli9k.png"
        style="width:50px; height:50px;">
      </button>
      <button name="col4" type=submit style="width:75px; height:75px; margin:0px">
        <img src="https://toppng.com/uploads/preview/red-circle-1155276042606ekqvli9k.png"
        style="width:50px; height:50px;">
      </button>
      <button name="col5" type=submit style="width:75px; height:75px; margin:0px">
        <img src="https://toppng.com/uploads/preview/red-circle-1155276042606ekqvli9k.png"
        style="width:50px; height:50px;">
      </button>
      <button name="col6" type=submit style="width:75px; height:75px; margin:0px">
        <img src="https://toppng.com/uploads/preview/red-circle-1155276042606ekqvli9k.png"
        style="width:50px; height:50px;">
      </button>
      <button name="col7" type=submit style="width:75px; height:75px; margin:0px">
        <img src="https://toppng.com/uploads/preview/red-circle-1155276042606ekqvli9k.png"
        style="width:50px; height:50px;">
      </button> </br>
      
    </form>

  </body>
  </html>
  
let () =
  Dream.run ~interface:"0.0.0.0"
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
