open Markdown

module Make_wrapped_html5
    (W : Xml_wrap.T)
    (Html5 : Html5_sigs.T
     with type +'a Xml.wrap = 'a W.t
      and type +'a wrap = 'a W.t
      and type +'a Xml.list_wrap = 'a W.tlist
      and type +'a list_wrap = 'a W.tlist)
 : sig

  val elm_to_html :
    render_pre:(kind:string ->
      string ->
      ([< Html5_types.li_content_fun
            > `Blockquote `H1 `H2 `H3 `H4 `H5 `H6 `Ol `P `Pre `Ul ]
       as 'a)
        Html5.elt) ->
    render_link:(href -> Html5_types.phrasing Html5.elt) ->
    render_img:(img_ref -> Html5_types.phrasing Html5.elt) ->
    paragraph -> 'a Html5.elt

  val par_text_to_html :
    render_link:(href -> Html5_types.phrasing Html5.elt) ->
    render_img:(img_ref -> Html5_types.phrasing Html5.elt) ->
    par_text -> Html5_types.phrasing Html5.elt Html5.list_wrap

  val text_to_html :
    render_link:(href -> Html5_types.phrasing Html5.elt) ->
    render_img:(img_ref -> Html5_types.phrasing Html5.elt) ->
    text -> Html5_types.phrasing Html5.elt

  val to_html :
    render_pre:(kind:string ->
      string ->
      ([< Html5_types.li_content_fun
            > `Blockquote `H1 `H2 `H3 `H4 `H5 `H6 `Ol `P `Pre `Ul ]
       as 'a)
        Html5.elt) ->
    render_link:(href -> Html5_types.phrasing Html5.elt) ->
    render_img:(img_ref -> Html5_types.phrasing Html5.elt) ->
    paragraph Html5.list_wrap -> 'a Html5.elt Html5.list_wrap

end


module Make_html5
    (Html5 : Html5_sigs.T
     with type +'a Xml.wrap = 'a
      and type +'a wrap = 'a
      and type +'a Xml.list_wrap = 'a list
      and type +'a list_wrap = 'a list)
  : module type of Make_wrapped_html5 (Xml_wrap.NoWrap) (Html5)
