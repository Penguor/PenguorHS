module Parser.Data where

import           Data.Text                      ( Text )

type State = [AddressFrame]

type AddressFrame = ([Text], Int, AddressType)

data AddressType = Byte


