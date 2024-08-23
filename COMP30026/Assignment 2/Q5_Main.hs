data Answer
  = Ambiguous String
  | Unambiguous
  deriving (Eq,Show)

part1, part2, part3 :: Answer

part4 :: String

part1 = Unambiguous

part2 = Unambiguous

part3 = Ambiguous "bbbbbaaaaaaaa"

part4 = "iptiptaea"