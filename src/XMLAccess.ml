
module type XML =
sig
  type xml

  val children : xml -> xml list

  val pcdata : xml -> string

  val tag : xml -> string
end


module F (X : XML) =
  struct

    let _mapOne node cond app =
      X.children node
      |> List.find cond
      |> app

    let _mapAll node cond app =
      X.children node
      |> List.filter cond
      |> List.map app

    let _tag n node =
      try
        n = X.tag node
      with
      | _ -> false

    let return x = x

    let findTags node name = _mapAll node (_tag name) return

    let ( $$? ) = findTags

    let findTag node name = _mapOne node (_tag name) return

    let ( $? ) = findTag

    let content node = _mapOne node (fun _ -> true) X.pcdata

    let ( $! ) node app = content node |> app

    let ( $$> ) nodes app = List.map app nodes
    let ( $$| ) nodes app = List.iter app nodes

    let ( $$->$? ) nodes name = nodes $$> fun node -> node $? name

  end
