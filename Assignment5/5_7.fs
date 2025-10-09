type typ =
  | TypI                                (* int                         *)
  | TypB                                (* bool                        *)
  | TypF of typ * typ                   (* (argumenttype, resulttype)  *)
  | TypL of typ