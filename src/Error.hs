module Error         ( Error(..) ) where

import GHC.Exception ( Exception )

data Error = HelpError
           | ArgumentParsingError String
             deriving Show
instance Exception Error
