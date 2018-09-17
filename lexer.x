{
module Main (main) where
}

%wrapper "basic"

$digit = 0-9			-- digits
$alpha = [a-zA-Z]		-- alphabetic characters

tokens :-

  $white+				;
  "--".*				;
  $digit+				{ \s -> Int (read s) }
  [\=\+\-\*\/\(\)\<\>]|\<=|>=	        { \s -> Sym s }

{
-- Each action has type :: String -> Token

-- The token type:
data Token =
	Sym String	|
	Int Int
	deriving (Eq,Show)

main::IO ()

main = do
  s <- getContents
  let x = alexScanTokens s
  mapM_ print x
}
