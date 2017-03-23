{-# LANGUAGE TemplateHaskell #-}
module QQ where
import GHC.Exts (IsString(..))
import Language.Haskell.TH.Quote

-- | QuasiQuoter for a non-interpolating IsString literal. The pattern portion is undefined.
s :: QuasiQuoter
s = QuasiQuoter ((\a -> [|fromString a|]) . filter (/= '\r'))
                 (Prelude.error "Cannot use q as a pattern")
                 (Prelude.error "Cannot use q as a type")
                 (Prelude.error "Cannot use q as a dec")
