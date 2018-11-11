import           Control.Monad                  ( unless )
import qualified UrlEncodedTest                 as UD

main :: IO ()
main = do
  putStrLn "Testing UrlDecode"
  res <- UD.tests
  unless res $ putStrLn "UrlDecode tests failed."
  putStrLn "Test suite not yet implemented"
