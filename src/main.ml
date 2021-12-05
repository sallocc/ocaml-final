(*let () =
  Dream.run ~interface:"0.0.0.0"
  @@ Dream.logger
  @@ Dream.router [
    Dream.get "/" (fun _ -> Dream.html Playground.welcome);
  ]
  @@ Dream.not_found *)
  
let show_form ?message request =
  <html>
  <body>

%   begin match message with
%   | None -> ()
%   | Some message ->
      <p>You entered: <b><%s message %>!</b></p>
%   end;

%   let redImage = "https://toppng.com/uploads/preview/red-circle-1155276042606ekqvli9k.png" in
%   let yellowImage = "https://www.pngfind.com/pngs/m/147-1479400_png-point-yellow-circle-transparent-background-png-download.png" in
%   let grayImage = "https://www.pngfind.com/pngs/m/180-1807233_location-dot-grey-grey-colour-circle-png-transparent.png" in

    <%s! Dream.form_tag ~action:"/" request %>
      
      <button name="col1" type=submit style="width:75px; height:75px;">
        <img src=<%s redImage%>
        style="width:50px; height:50px; margin:0px;">
      </button>
      <button name="col2" type=submit style="width:75px; height:75px; margin:0px">
        <img src=<%s grayImage%>
        style="width:50px; height:50px; margin:0px;">
      </button>
      <button name="col3" type=submit style="width:75px; height:75px; margin:0px">
        <img src="https://toppng.com/uploads/preview/red-circle-1155276042606ekqvli9k.png"
        style="width:50px; height:50px; margin:0px;">
      </button>
      <button name="col4" type=submit style="width:75px; height:75px; margin:0px">
        <img src="https://toppng.com/uploads/preview/red-circle-1155276042606ekqvli9k.png"
        style="width:50px; height:50px; margin:0px;">
      </button>
      <button name="col5" type=submit style="width:75px; height:75px; margin:0px">
        <img src="https://toppng.com/uploads/preview/red-circle-1155276042606ekqvli9k.png"
        style="width:50px; height:50px; margin:0px;">
      </button>
      <button name="col6" type=submit style="width:75px; height:75px; margin:0px">
        <img src=<%s yellowImage%>
        style="width:50px; height:50px; margin:0px;">
      </button>
      <button name="col7" type=submit style="width:75px; height:75px; margin:0px">
        <img src="https://toppng.com/uploads/preview/red-circle-1155276042606ekqvli9k.png"
        style="width:50px; height:50px; margin:0px;">
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
        | `Ok ["col1", _] ->
          Dream.html (show_form ~message:"Clicked column 1" request)
        | `Ok ["col2", _] ->
          Dream.html (show_form ~message:"Clicked column 2" request)
        | `Ok ["col3", _] -> 
          Dream.html (show_form ~message:"Clicked column 3" request)
        | `Ok ["col4", _] ->
          Dream.html (show_form ~message:"Clicked column 4" request)
        | `Ok ["col5", _] ->
          Dream.html (show_form ~message:"Clicked column 5" request)
        | `Ok ["col6", _] ->
          Dream.html (show_form ~message:"Clicked column 6" request)
        | `Ok ["col7", _] ->
          Dream.html (show_form ~message:"Clicked column 7" request)
        | _ ->
          Dream.empty `Bad_Request);

  ]
  @@ Dream.not_found
  
