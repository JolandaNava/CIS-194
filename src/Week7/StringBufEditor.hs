module Week7.StringBufEditor where
-- module was called Main

import Week7.Editor
import Week7.StringBuffer

main = runEditor editor $ unlines
         [ "This buffer is for notes you don't want to save, and for"
         , "evaluation of steam valve coefficients."
         , "To load a different file, type the character L followed"
         , "by the name of the file."
         ]