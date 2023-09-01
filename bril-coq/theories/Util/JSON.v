From Coq Require Import Lists.List Strings.String.
From ExtLib Require Import EitherMonad.
From JSON Require Import JSON.

Import IfNotations.

Open Scope string_scope.

(** [member key j] is the value associated with key [key] of [json] *)
Definition member (key : string) (j : json) : string + json :=
  if j is JSON__Object entries then
    match List.find (fun '(key', _) => key =? key') entries with
    | Some (_, value) => ret value
    | None => inl ("JSON object does not have property " ++ key)
    end
  else
    inl "Cannot extract member from non-object".

(** [get_string j] is the string wrapped by json [j] *)
Definition get_string (j : json) : string + string :=
  if j is JSON__String s then ret s else inl "Expected string".
