(* Copyright (C) 2009 Mauricio Fernandez <mfp@acm.org> *)

module Make_html5 (Html5 : Html5_sigs.T) = struct

open Markdown
open Html5
module W = Xml.W

let pcdata_w x = W.return @@ pcdata @@ W.return x
let wrap_list f l =
  List.fold_right
    (fun x l -> W.(cons (return @@ f x) l))
    l (W.nil ())

let rec elm_to_html ~render_pre ~render_link ~render_img elm =
  let self = elm_to_html ~render_pre ~render_link ~render_img in
  let item l = li (wrap_list self l)

  in match elm with
      Normal text -> p (par_text_to_html ~render_link ~render_img text)
    | Pre (s, kind) -> begin match kind with
          Some k -> render_pre ~kind:k s
        | None -> pre W.(singleton @@ pcdata_w s)
      end
    | Heading (l, text) ->
        let f =
          match l with 1 -> h1 | 2 -> h2 | 3 -> h3 | 4 -> h4 | 5 -> h5 | _ -> h6
        in f (par_text_to_html render_link render_img text)
    | Quote ps -> blockquote @@ wrap_list self ps
    | Ulist (fst, others) ->
        ul (wrap_list item (fst :: others))
    | Olist (fst, others) ->
        ol (wrap_list item (fst :: others))

and par_text_to_html ~render_link ~render_img =
  wrap_list (text_to_html ~render_link ~render_img)

and text_to_html ~render_link ~render_img = function
    Text s -> pcdata @@ W.return s
  | Emph s -> em @@ W.singleton @@ pcdata_w s
  | Bold s -> b @@ W.singleton @@ pcdata_w s
  | Struck l -> del (wrap_list (text_to_html ~render_link ~render_img) l)
  | Code s -> code @@ W.singleton @@ pcdata_w s
  | Anchor id ->
      (*  would like to do
            a ~a:[XHTML.M_01_00.a_name_01_00 id] []
          but that'd require switching to M_01_00 everywhere, so cheap hack *)
      b ~a:[a_id @@ W.return id] (W.nil ())
  | Link href -> begin match href.href_target with
        s when String.length s >= 1 && s.[0] = '#' ->
          a ~a:[a_href (W.return @@ uri_of_string s)]
            @@ W.singleton @@ pcdata_w href.href_desc
      | _ -> render_link href
    end
  | Image href -> render_img href

let to_html ~render_pre ~render_link ~render_img l =
  W.map (elm_to_html ~render_pre ~render_link ~render_img) l

end
