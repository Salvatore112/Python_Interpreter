(** Copyright 2021-2023, Averin Pavel *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

(*Standart data types: integers, strings, lists*)
type value =
  | Int of int
  | String of string
  | List of value list

(*Standart arithmetic operations *)
type arith_op =
  | Add
  | Sub
  | Mul
  | Div
  | Mod

(*Funcions' name & args' name*)
type identifier = Identifier of string

(*Standart boolean operators*)
type bool_op =
  | And
  | Or
  | Equal

(*Standart expressions*)
type expression =
  | Const of value
  | Variable of identifier
  | ArithOp of arith_op * expression * expression
  | BoolOp of bool_op * expression * expression
  | FunctionCall of identifier * expression list
  

type statement =
  | Expression of expression
  | IfElse of expression * statement list * statement list
  | While of expression * statement list
  | Assign of expression * expression
  | Return of expression
  | Function of identifier * identifier list * statement list
