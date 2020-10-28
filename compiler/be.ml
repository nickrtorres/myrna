open Printf

let context_struct name =
  let ident = "    " in
  printf
    "pub struct %s <'a> {\n%sstate: Box<dyn State>,\n%scandidate: &'a str,\n}\n"
    name ident ident

let context_impl_from name entry =
  let single_indent = "    " in
  let double_indent = "        " in
  printf
    "impl<'a> %s <'a> {\n\
     %spub fn from(candidate: &'a str) -> Self {\n\
     %s%s {\n\
     %sstate: Box::new(%s {}),\n\
     %scandidate,\n\
     %s}\n"
    name single_indent double_indent name double_indent entry double_indent
    single_indent

let context_impl_accepts transitions =
  let single_indent = "    " in
  let double_indent = "        " in
  let triple_indent = "            " in
  let transition_string (k, v) =
    printf "%s\'%s\' => self.state = self.state.accept_%s(),\n" triple_indent v
      k
  in

  printf "%spub fn accepts(mut self) -> bool {\n" single_indent;
  printf "%sfor c in self.candidate.chars() {\n" double_indent;
  List.iter transition_string transitions;
  printf "%s}\n" double_indent;
  printf "%sself.state.accepting()\n" double_indent;
  printf "%s}\n" single_indent

let state_declarations states =
  List.iter (fun state -> printf "struct %s;\n" state) states

let state_interface transitions =
  let single_indent = "    " in
  let acceptor transition =
    printf "%sfn accept_%s(&self) -> Box<dyn State>;\n" single_indent transition
  in
  printf "trait State {\n";
  List.iter acceptor transitions;
  printf "}\n"

(*iden (key, value) *)
let interface_implementations states =
  let interface_impl (state, transition) =
    let key, value = transition in
    let single_indent = "    " in
    let double_indent = "        " in
    printf "impl State for %s {\n" state;
    printf "%sfn accept_%s(&self) -> Box<dyn State> {\n" single_indent key;
    printf "%sBox::new(%s {})\n" double_indent value;
    printf "%s}\n" single_indent;
    printf "}\n"
  in

  List.iter interface_impl states
