module IR where

import Parser 

data IR = IR [IRToken]

data IRToken = SYSTEM

astToir :: Program -> IR
astToir p = IR [SYSTEM]