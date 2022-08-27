module PP where

-- | Prepend a string with a number of spaces
indent :: Int -> String -> String
indent n s = replicate n ' ' ++ s

-- | Prepend with spaces and insert a linebreak
indentLn :: Int -> String -> String
indentLn n s = replicate n ' ' ++ s ++ "\n"

-- | Prepend spaces to each line
indentAll :: Int -> [String] -> [String]
indentAll n strs = fmap (indent n) strs

-- | Prepend spaces and add a linebreak to each line
indentLnAll :: Int -> [String] -> [String]
indentLnAll n strs = fmap (indentLn n) strs

-- | Given string @new@ and @lines@, prepend the first line
-- with @new@ and indent the rest by @length new@
prependBlock :: String -> [String] -> [String]
prependBlock new [] = []
prependBlock new (str : rest) =
  (new ++ str) : indentAll (length new) rest
