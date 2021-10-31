module Engine.Util where

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Sequence
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Quote as TH

forMapM_ ::
    (Applicative m) => Map.Map k v -> (k -> v -> m ()) -> m ()
forMapM_ xs f =
    Map.foldrWithKey
        (\k v c -> f k v *> c)
        (pure ())
        xs

forSequenceM_ ::
    (Applicative m) => Sequence.Seq v -> (Int -> v -> m ()) -> m ()
forSequenceM_ xs f =
    Sequence.foldrWithIndex
        (\i v c -> f i v *> c)
        (pure ())
        xs

longString :: TH.QuasiQuoter
longString = TH.QuasiQuoter
    { quoteExp = \str -> pure $ TH.LitE $ TH.StringL str
    , quotePat = \_ -> fail "not supported"
    , quoteType = \_ -> fail "not supported"
    , quoteDec = \_ -> fail "not supported"
    }

fromFile :: TH.QuasiQuoter
fromFile = TH.quoteFile longString
