module Ether where

maybeToEither :: Maybe a -> String -> Either String a
maybeToEither x msg =
  case x of
    Nothing -> Left msg
    Just x' -> Right x'

forceRight :: Either String b -> b
forceRight x =
  case x of
    Left msg -> error msg
    Right x' -> x'

eitherToBool :: Either a b -> Bool
eitherToBool x =
  case x of
    Left  _ -> False
    Right _ -> True

ifRight :: Either a b -> (b -> Either a c) -> Either a c
ifRight x fn =
  case x of
    Left  l -> Left l
    Right r -> fn r

ifRight2 :: Either a b -> Either a b -> (b -> b -> Either a c) -> Either a c
ifRight2 x y fn =
  ifRight x (\x' ->
    ifRight y (\y' ->
      fn x' y'))

fail :: String -> Either String b
fail msg = Left msg

eitherToString :: (Show a, Show b) => Either a b -> String
eitherToString x =
  case x of
    Left  l -> show l
    Right r -> show r
