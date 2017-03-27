module Chapter5.Prettify where
  import Chapter5.SimpleJSON

  data Doc = ToBeDefined
             deriving (Show)

  string :: String -> Doc
  string str = undefined

  text :: String -> Doc
  text str = undefined

  double :: String -> Doc
  double str = undefined
