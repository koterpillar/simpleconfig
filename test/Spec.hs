import Data.Text

import Config.Simple

data ConfigF k = Config
  { _address :: ConfigItem Text k
  , _dryRun :: ConfigBool k
  , _widgets :: ConfigSet Text k
  }

type Config = Complete ConfigF

type PartialConfig = Partial ConfigF

main :: IO ()
main = putStrLn "Test suite not yet implemented"
