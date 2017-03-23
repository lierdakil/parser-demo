{-
This module's license, see https://hackage.haskell.org/package/string-quote-0.0.1
Copyright 2009 by Audrey Tang

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to
deal in the Software without restriction, including without limitation the
rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
sell copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
-}
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
