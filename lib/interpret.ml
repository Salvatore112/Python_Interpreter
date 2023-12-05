(** Copyright 2021-2022, Averin Pavel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast

module type MONAD = sig
  type 'a t

  val return : 'a -> 'a t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t

  (* A synonym for >>= *)
  val ( let* ) : 'a t -> ('a -> 'b t) -> 'b t
end

module type MONADERROR = sig
  include MONAD

  val error : string -> 'a t
end

module Result : MONADERROR with type 'a t = ('a, string) result = struct
  type 'a t = ('a, string) Result.t

  let ( >>= ) = Result.bind
  let return = Result.ok
  let error = Result.error
  let ( let* ) = ( >>= )
end

module Eval (M : MONADERROR) = struct
  open M

  let rec fold_left func init = function
    | [] -> return init
    | hd :: tl -> func init hd >>= fun init -> fold_left func init tl
  ;;

  (* Environment symbols *)
  type var_symb =
    { identifier : identifier
    ; value : value
    }

  type function_symb =
    { identifier : identifier
    ; params : identifier list
    ; body : statement list
    }
  (* Environments *)

  type environment =
    { id : identifier
    ; local_envs : environment list
    ; functions : function_symb list
    ; flag : flag
    ; return_v : value
    ; vars : var_symb list
    }

  let local_env : environment =
    { id = Identifier "local"
    ; local_envs = []
    ; functions = []
    ; flag = No
    ; return_v = None
    ; vars = []
    }
  ;;

  let gloval_env =
    { id = Identifier "global"
    ; vars = []
    ; local_envs = [ local_env ]
    ; functions = []
    ; flag = No
    ; return_v = None
    }
  ;;

  (* Environment related functions for variables *)

  let var_in_env i env = List.exists (fun (x : var_symb) -> x.identifier = i) env.vars

  let change_var_in_env i new_value env =
    let new_vars =
      List.map
        (fun (x : var_symb) ->
          if x.identifier = i then { x with value = new_value } else x)
        env.vars
    in
    { env with vars = new_vars }
  ;;

  let change_or_add_val_in_env env (x : var_symb) =
    match var_in_env x.identifier env with
    | true -> change_var_in_env x.identifier x.value env
    | false -> { env with vars = x :: env.vars }
  ;;

  let change_or_add_var_list_in_env = List.fold_left change_or_add_val_in_env
  let get_var_from_env i env = List.find (fun (x : var_symb) -> x.identifier = i) env.vars

  (* Environment related functions for functions *)

  let add_params_to_func_env params func_env =
    let new_params = List.map (fun (x : identifier) -> x) params in
    { func_env with params = new_params }
  ;;

  let func_in_env i env =
    List.exists (fun (a : function_symb) -> i = a.identifier) env.functions
  ;;

  let change_func_in_env new_func env =
    let new_funcs =
      List.map
        (fun (x : function_symb) ->
          if x.identifier = new_func.identifier
          then { x with body = new_func.body; params = new_func.params }
          else x)
        env.functions
    in
    { env with functions = new_funcs }
  ;;

  let change_or_add_func_in_env (x : function_symb) env =
    match func_in_env x.identifier env with
    | true -> change_func_in_env x env
    | false -> { env with functions = x :: env.functions }
  ;;

  let get_func_from_env i env =
    List.find (fun (x : function_symb) -> x.identifier = i) env.functions
  ;;

  (* Miscellaneous *)
  let pack_to_string = function
    | String a -> a
    | Int a -> Int.to_string a
    | Bool a -> Bool.to_string a
    | _ -> "Interpretation Error"
  ;;

  let rec print_list = function
    | [] -> ()
    | e :: l ->
      print_string e;
      print_string " ";
      print_list l
  ;;

  let unpack = function
    | Ok x -> x
    | _ -> failwith "Failed to unpack"
  ;;

  let rec map1 f = function
    | [] -> return []
    | h :: tl -> f h >>= fun c -> map1 f tl >>= fun lst -> return (c :: lst)
  ;;

  let combine_args_and_params args (params : value list) =
    List.map (fun x -> { identifier = fst x; value = snd x }) (List.combine args params)
  ;;

  (* Debugging & Testing functions *)

  let get_str_from_identifier (Identifier i) = i

  let rec print_funcs = function
    | [] -> ()
    | func :: remaining_functions ->
      print_string (get_str_from_identifier func.identifier);
      print_string " ";
      print_funcs remaining_functions
  ;;

  let rec print_vars = function
    | [] -> ()
    | (var : var_symb) :: (remaining_vars : var_symb list) ->
      print_string (get_str_from_identifier var.identifier);
      print_string " ";
      print_vars remaining_vars
  ;;

  type dispatch =
    { i_expr : dispatch -> environment -> expression -> value t
    ; i_stmt : dispatch -> environment -> statement -> environment t
    }

  let make_monad_str_list_from_exp_list exp_list i_exp_or_stmt env =
    List.map
      (fun e ->
        i_exp_or_stmt.i_expr i_exp_or_stmt env e
        >>= fun value -> return (pack_to_string value))
      exp_list
  ;;

  (** let merge_args_and_params args param **)
  let i_exp_or_stmt =
    let rec i_expr (exp_or_stmt : dispatch) (env : environment) exp =
      let rec apply env body =
        if env.flag == Return_f
        then return env.return_v
        else (
          match body with
          | [] -> return None
          | hd :: tl ->
            let* a = exp_or_stmt.i_stmt exp_or_stmt env hd in
            apply a tl)
      in
      match exp with
      | Const a -> return a
      | ArithOp (Add, a, b) ->
        let* a = i_expr exp_or_stmt env a in
        let* b = i_expr exp_or_stmt env b in
        (match a, b with
         | Int a1, Int b1 -> return (Int (a1 + b1))
         | _ -> error "unexpected type")
      | ArithOp (Sub, a, b) ->
        let* a = i_expr exp_or_stmt env a in
        let* b = i_expr exp_or_stmt env b in
        (match a, b with
         | Int a1, Int b1 -> return (Int (a1 - b1))
         | _ -> error "unexpected type")
      | ArithOp (Div, a, b) ->
        let* a = i_expr exp_or_stmt env a in
        let* b = i_expr exp_or_stmt env b in
        (match a, b with
         | _, Int b1 when b1 = 0 -> error "0 Division Error"
         | Int a1, Int b1 -> return (Int (a1 / b1))
         | _ -> error "unexpected type")
      | ArithOp (Mul, a, b) ->
        let* a = i_expr exp_or_stmt env a in
        let* b = i_expr exp_or_stmt env b in
        (match a, b with
         | Int a1, Int b1 -> return (Int (a1 * b1))
         | _ -> error "unexpected type")
      | BoolOp (Equal, a, b) ->
        let* a = i_expr exp_or_stmt env a in
        let* b = i_expr exp_or_stmt env b in
        return (Bool (a = b))
      | BoolOp (NotEqual, a, b) ->
        let* a = i_expr exp_or_stmt env a in
        let* b = i_expr exp_or_stmt env b in
        return (Bool (a != b))
      | BoolOp (Less, a, b) ->
        let* a = i_expr exp_or_stmt env a in
        let* b = i_expr exp_or_stmt env b in
        return (Bool (a < b))
      | BoolOp (LessOrEqual, a, b) ->
        let* a = i_expr exp_or_stmt env a in
        let* b = i_expr exp_or_stmt env b in
        return (Bool (a <= b))
      | BoolOp (Greater, a, b) ->
        let* a = i_expr exp_or_stmt env a in
        let* b = i_expr exp_or_stmt env b in
        return (Bool (a > b))
      | BoolOp (GreaterOrEqual, a, b) ->
        let* a = i_expr exp_or_stmt env a in
        let* b = i_expr exp_or_stmt env b in
        return (Bool (a >= b))
      | BoolOp (And, a, b) ->
        let* a = i_expr exp_or_stmt env a in
        let* b = i_expr exp_or_stmt env b in
        return (Bool (a = b))
      | BoolOp (Or, a, b) ->
        let* a = i_expr exp_or_stmt env a in
        let* b = i_expr exp_or_stmt env b in
        return (Bool (a = Bool true || b = Bool true))
      | Variable i ->
        (match var_in_env i env with
         | true -> return (get_var_from_env i env).value
         | false -> error "undefined Variable")
      | FunctionCall (identifier, exp_list) ->
        (match func_in_env identifier env with
         | false -> error "undefined Function"
         | true ->
           let* x = map1 (fun x -> i_expr exp_or_stmt env x) exp_list in
           apply
             { (change_or_add_var_list_in_env
                  env
                  (combine_args_and_params (get_func_from_env identifier env).params x))
               with
               id = Identifier "local"
             ; local_envs = [ { local_env with vars = env.vars } ]
             ; functions = [ get_func_from_env identifier env ]
             }
             (get_func_from_env identifier env).body)
      | _ -> error "wip"
    in
    let rec i_stmt (i_exp_or_stmt : dispatch) (env : environment) = function
      | Return exp ->
        let* value = i_exp_or_stmt.i_expr i_exp_or_stmt env exp in
        return { env with flag = Return_f; return_v = value }
      | Assign (l, r) ->
        (match l with
         | Variable identifier ->
           let* value = i_exp_or_stmt.i_expr i_exp_or_stmt env r in
           return (change_or_add_val_in_env env { identifier; value })
         | _ -> error "Left-hand side operator is not a variable")
      | Function (i, some_params, some_body) ->
        (match i with
         | Identifier "print" -> return env
         | Identifier id ->
           let new_func_env =
             { identifier = i; params = some_params; body = some_body }
           in
           return (change_or_add_func_in_env new_func_env env)
         | _ -> error "Unsupported identifier")
      | _ -> error "wip"
    in
    { i_expr; i_stmt }
  ;;

  let interpret_exp (e : expression) global_env =
    i_exp_or_stmt.i_expr i_exp_or_stmt global_env e
  ;;

  let get_env global_env = fold_left (i_exp_or_stmt.i_stmt i_exp_or_stmt) global_env
end

open Eval (Result)
