module Text.XML.Hexml.Lens.Lite (
  _XML,
  _contents,
  _inner,
  _outer,
  textContents,
  _Attribute,
  iattributes,
  nodes,
  node,
  _children,
) where

import Control.Lens hiding (children)
import Data.ByteString.Char8 qualified as BS
import Text.XML.Hexml

_XML :: Prism' BS.ByteString Node
{-# INLINE _XML #-}
_XML = prism' outer doParse
  where
    doParse x =
      case parse x of
        Right n -> Just $
          case children n of
            [y] -> y
            _ -> n
        Left _ -> Nothing

_contents :: Fold Node (Either BS.ByteString Node)
{-# INLINE _contents #-}
_contents = folding contents

_inner, _outer, textContents :: Fold Node BS.ByteString
_inner = to inner
{-# INLINE _inner #-}
_outer = to outer
{-# INLINE _outer #-}
textContents = folding contents . _Left
{-# INLINE textContents #-}

_Attribute :: BS.ByteString -> Getter Node (Maybe BS.ByteString)
{-# INLINE _Attribute #-}
_Attribute n = pre $ to (`attributeBy` n) . folded . to attributeValue

iattributes :: IndexedFold BS.ByteString Node BS.ByteString
{-# INLINE iattributes #-}
iattributes = ifolding (map (\(Attribute n v) -> (n, v)) . attributes)

nodes :: BS.ByteString -> Getter Node [Node]
{-# INLINE nodes #-}
nodes name_ = to $ flip childrenBy name_

node :: BS.ByteString -> Fold Node Node
{-# INLINE node #-}
node n = nodes n . folded

_children :: Getter Node [Node]
_children = to children
