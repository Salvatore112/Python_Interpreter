
  $ ./REPL.exe
  $ ./REPL.exe -help
  $ ./REPL.exe -cbv - <<EOF
  > \f.x
  $ ./REPL.exe -no - <<EOF
  > (\x.\y.x)(\u.u)((\x. x x)(\x.x x))
