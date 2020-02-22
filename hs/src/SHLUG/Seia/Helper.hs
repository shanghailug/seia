module SHLUG.Seia.Helper where


-- show short string
sss :: Show a => Int -> a -> String
sss l x = if length a <= l then a else take (l - 4) a ++ " ..." where
  a = show x
