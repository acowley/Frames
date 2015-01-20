{-# LANGUAGE DataKinds, FlexibleContexts, OverloadedStrings,
             TemplateHaskell, TypeOperators #-}

-- This is a loose port of a
-- [[http://ajkl.github.io/Dataframes/][dataframe tutorial]] Rosetta
-- Stone to compare traditional dataframe tools built in R, Julia,
-- Python, etc. with ~Frames~.

-- The example data file used does not include column headers, nor
-- does it use commas to separate values, so it does not fall into the
-- sweet spot of CSV parsing that ~Frames~ is aimed at. That said,
-- this mismatch of test data and library support is a great
-- opportunity to verify that ~Frames~ are flexible enough to meet a
-- variety of needs.

-- We begin with rather a lot of imports to support a variety of test
-- operations and parser customization. I encourage you to start with a
-- smaller test program than this!

import Control.Applicative
import qualified Control.Foldl as L
import qualified Data.Foldable as F
import Lens.Family
import Frames
import Frames.CSV (tableTypesOpt, readTableOpt)
import Pipes hiding (Proxy)
import qualified Pipes.Prelude as P

-- A few other imports will be used for highly customized parsing [[Better Types][later]].

import Data.Proxy
import Frames.CSV (tableTypesPrefixedOpt')
import TutorialUsers

-- * Data Import

-- We usually package column names with the data to keep things a bit
-- more self-documenting. This might mean adding a row to the data
-- file with the column names, but, if we must use a particular data
-- file whose column names are provided in a separate specification,
-- we can override the default parsing options.

-- The data set this example considers is rather far from the sweet spot
-- of CSV processing that ~Frames~ is aimed it: it does not include
-- column headers, nor does it use commas to separate values! However,
-- these mismatches do provide an opportunity to see that ~Frames~ are
-- flexible enough to meet a variety of needs.

tableTypesOpt  ["user id", "age", "gender", "occupation", "zip code"]
               "|" "Users" "data/ml-100k/u.user"

-- This template haskell splice explicitly specifies column names, a
-- separator string, the name for the inferred record type, and the
-- data file from which to infer the record type. The result of this
-- splice is included in an [[* Splice Dump][appendix]] below so you
-- can flip between the generated code and how it is used.

-- We can load the module into =cabal repl= to see what we have so far.

-- #+BEGIN_EXAMPLE
-- λ> :i Users
-- type Users =
--   Rec
--     '["user id" :-> Int, "age" :-> Int, "gender" :-> Text,
--       "occupation" :-> Text, "zip code" :-> Text]
-- #+END_EXAMPLE

-- This lets us perform a quick check that the types are basically what
-- we expect them to be.

-- We now define a streaming representation of the full data set. If the
-- data set is too large to keep in memory, we can process it as it
-- streams through RAM.

movieStream :: Producer Users IO ()
movieStream = readTableOpt usersParser "data/ml-100k/u.user"

-- Alternately, if we want to run multiple operations against a data set
-- that /can/ fit in RAM, we can do that. Here we define an in-core (in
-- memory) array of structures (AoS) representation.

loadMovies :: IO (Frame Users)
loadMovies = inCoreAoS movieStream

-- ** Streaming Cores?

-- A ~Frame~ is an in-memory representation of your data. The ~Frames~
-- library stores each column as compactly as it knows how, and lets you
-- index your data as a structure of arrays (where each field of the
-- structure is an array corresponding to a column of your data), or as
-- an array of structures, also known as a ~Frame~. These latter
-- structures correspond to rows of your data, and rows of data may be
-- handled in a streaming fashion so that you are not limited to
-- available RAM. In the streaming paradigm, you process each row
-- individually as a single record.

-- A ~Frame~ provides ~O(1)~ indexing, as well as any other operations
-- you are familiar with based on the ~Foldable~ class. If a data set is
-- small, keeping it in RAM is usually the fastest way to perform
-- multiple analyses on that data that you can't fuse into a single
-- traversal.

-- Alternatively, a ~Producer~ of rows is a great way to whittle down a
-- large data set before moving on to whatever you want to do next.

-- ** Sanity Check

-- We can compute some easy statistics to see how things look.

-- #+BEGIN_EXAMPLE
-- λ> ms <- loadMovies
-- λ> L.fold L.minimum (view age <$> ms)
-- Just 7
-- #+END_EXAMPLE

-- When there are multiple properties we would like to compute, we can
-- fuse multiple traversals into one pass using something like the [[http://hackage.haskell.org/package/foldl][foldl]]
-- package

-- #+BEGIN_EXAMPLE
-- λ> L.fold (L.pretraverse age ((,) <$> L.minimum <*> L.maximum)) ms
-- (Just 7,Just 73)
-- #+END_EXAMPLE

-- Here we are projecting the =age= column out of each record, and
-- computing the minimum and maximum =age= for all rows.

-- * Subsetting

-- ** Row Subset

-- Data may be inspected using either Haskell's traditional list API...

-- #+BEGIN_EXAMPLE
-- λ> mapM_ print (take 3 (F.toList ms))
-- {user id :-> 1, age :-> 24, gender :-> "M", occupation :-> "technician", zip code :-> "85711"}
-- {user id :-> 2, age :-> 53, gender :-> "F", occupation :-> "other", zip code :-> "94043"}
-- {user id :-> 3, age :-> 23, gender :-> "M", occupation :-> "writer", zip code :-> "32067"}
-- #+END_EXAMPLE

-- ... or =O(1)= indexing of individual rows. Here we take the last three
-- rows of the data set,

-- #+BEGIN_EXAMPLE
-- λ> mapM_ (print . frameRow ms) [frameLength ms - 3 .. frameLength ms - 1]
-- {user id :-> 941, age :-> 20, gender :-> "M", occupation :-> "student", zip code :-> "97229"}
-- {user id :-> 942, age :-> 48, gender :-> "F", occupation :-> "librarian", zip code :-> "78209"}
-- {user id :-> 943, age :-> 22, gender :-> "M", occupation :-> "student", zip code :-> "77841"}
-- #+END_EXAMPLE

-- This lets us view a subset of rows,

-- #+BEGIN_EXAMPLE
-- λ> mapM_ (print . frameRow ms) [50..55]
-- {user id :-> 51, age :-> 28, gender :-> "M", occupation :-> "educator", zip code :-> "16509"}
-- {user id :-> 52, age :-> 18, gender :-> "F", occupation :-> "student", zip code :-> "55105"}
-- {user id :-> 53, age :-> 26, gender :-> "M", occupation :-> "programmer", zip code :-> "55414"}
-- {user id :-> 54, age :-> 22, gender :-> "M", occupation :-> "executive", zip code :-> "66315"}
-- {user id :-> 55, age :-> 37, gender :-> "M", occupation :-> "programmer", zip code :-> "01331"}
-- {user id :-> 56, age :-> 25, gender :-> "M", occupation :-> "librarian", zip code :-> "46260"}
-- #+END_EXAMPLE

-- ** Column Subset

-- We can consider a single column.

-- #+BEGIN_EXAMPLE
-- λ> take 6 $ F.foldMap ((:[]) . view occupation) ms
-- ["technician","other","writer","technician","other","executive"]
-- #+END_EXAMPLE

-- Or multiple columns,

miniUser :: Users -> Rec [Occupation, Gender, Age]
miniUser = rcast

-- #+BEGIN_EXAMPLE
-- λ> mapM_ print . take 6 . F.toList $ fmap miniUser ms
-- {occupation :-> "technician", gender :-> "M", age :-> 24}
-- {occupation :-> "other", gender :-> "F", age :-> 53}
-- {occupation :-> "writer", gender :-> "M", age :-> 23}
-- {occupation :-> "technician", gender :-> "M", age :-> 24}
-- {occupation :-> "other", gender :-> "F", age :-> 33}
-- {occupation :-> "executive", gender :-> "M", age :-> 42}
-- #+END_EXAMPLE

-- ** Query / Conditional Subset

-- Filtering our frame is rather nicely done using the
-- [[http://hackage.haskell.org/package/pipes][pipes]] package. Here
-- we pick out the users whose occupation is "writer".

writers :: (Occupation ∈ rs, Monad m) => Pipe (Rec rs) (Rec rs) m r
writers = P.filter ((== "writer") . view occupation)

-- #+BEGIN_EXAMPLE
-- λ> runEffect $ movieStream >-> writers >-> P.take 6 >-> P.print
-- {user id :-> 3, age :-> 23, gender :-> "M", occupation :-> "writer", zip code :-> "32067"}
-- {user id :-> 21, age :-> 26, gender :-> "M", occupation :-> "writer", zip code :-> "30068"}
-- {user id :-> 22, age :-> 25, gender :-> "M", occupation :-> "writer", zip code :-> "40206"}
-- {user id :-> 28, age :-> 32, gender :-> "M", occupation :-> "writer", zip code :-> "55369"}
-- {user id :-> 50, age :-> 21, gender :-> "M", occupation :-> "writer", zip code :-> "52245"}
-- {user id :-> 122, age :-> 32, gender :-> "F", occupation :-> "writer", zip code :-> "22206"}
-- #+END_EXAMPLE

-- * Better Types

-- A common disappointment of parsing general data sets is the
-- reliance on text for data representation even /after/ parsing. If
-- you find that the default =ColType= spectrum of potential column
-- types that =Frames= uses doesn't capture desired structure, you can
-- go ahead and define your own universe of column types! The =Users=
-- row types we've been playing with here is rather boring: it only
-- uses =Int= and =Text= column types. But =Text= is far too vague a
-- type for a column like =gender=.

-- #+BEGIN_EXAMPLE
-- λ> L.purely P.fold L.nub (movieStream >-> P.map (view gender))
-- ["M", "F"]
-- #+END_EXAMPLE

-- Aha! The =gender= column is pretty simple, so let's go ahead and
-- define our own universe of column types.

-- -- #+BEGIN_SRC haskell
-- data GenderT = Male | Female deriving (Eq,Show,Typeable)

-- type instance VectorFor GenderT = V.Vector

-- instance Readable GenderT where
--   fromText "M" = return Male
--   fromText "F" = return Female
--   fromText _ = mzero

-- type MyColumns = GenderT ': CommonColumns
-- #+END_SRC

-- The full code for the custom type may be found in an [[* User Types][appendix]]. Note
-- that it must be defined in a separate module to satisfy GHC's stage
-- restrictions related to Template Haskell.

-- We name this record type ~U2~, and give all the generated column types
-- and lenses a prefix, "u2", so they don't conflict with the definitions
-- we generated earlier.

tableTypesPrefixedOpt' (Proxy :: Proxy (ColumnUniverse MyColumns))
                       ["user id", "age", "gender", "occupation", "zip code"]
                       "|" "U2" "u2" "data/ml-100k/u.user"

movieStream2 :: Producer U2 IO ()
movieStream2 = readTableOpt u2Parser "data/ml-100k/u.user"

-- This new record type, =U2=, has a more interesting =gender=
-- column.

-- #+BEGIN_EXAMPLE
-- λ> :i U2
-- type U2 =
--   Rec
--     '["user id" :-> Int, "age" :-> Int, "gender" :-> GenderT,
--       "occupation" :-> Text, "zip code" :-> Text]
-- #+END_EXAMPLE

-- Let's take the occupations of the first 10 male users:

maleOccupations :: (U2gender ∈ rs, U2occupation ∈ rs, Monad m)
                => Pipe (Rec rs) Text m r
maleOccupations = P.filter ((== Male) . view u2gender)
                  >-> P.map (view u2occupation)

-- #+BEGIN_EXAMPLE
-- λ> runEffect $ movieStream2 >-> maleOccupations >-> P.take 10 >-> P.print
-- "technician"
-- "writer"
-- "technician"
-- "executive"
-- "administrator"
-- "administrator"
-- "student"
-- "lawyer"
-- "educator"
-- "scientist"
-- #+END_EXAMPLE

-- So there we go! We've done both row and column subset queries with a
-- strongly typed query (namely, ~(== Female)~). Another situation in
-- which one might want to define a custom universe of column types is
-- when dealing with dates. This would let you both reject rows with
-- badly formatted dates, for example, and efficiently query the data set
-- with richly-typed queries.

-- Even better, did you notice the types of ~writers~ and
-- ~femaleOccupations~? They are polymorphic over the full row type!
-- That's what the ~(Occupation ∈ rs)~ constraint signifies: such a
-- function will work for record types with any set of fields, ~rs~, so
-- long as ~Occupation~ is an element of that set. This means that if
-- your schema changes, or you switch to a related but different data
-- set, *these functions can still be used without even touching the
-- code*. Just recompile against the new data set, and you're good to go.

-- * Appendix
-- ** User Types

-- Here are the definitions needed to define the ~UserCol~ type with its
-- more descriptive ~GenderT~ type. We have to define these things in a
-- separate module from our main work due to GHC's stage restrictions
-- regarding Template Haskell. Specifically, ~UserCol~ and its instances
-- are used at compile time to infer the record type needed to represent
-- the data file. Notice the extension point here, though somewhat
-- buried, is not too rough: you prepend new, more refined, type
-- compatibility checks to the end of the ~inferType~ definition.

-- This may not be something you'd want to do for every data
-- set. However, the /ability/ to refine the structure of parsed data
-- is in keeping with the overall goal of ~Frames~: it's easy to take
-- off, and the sky's the limit.

-- -- #+begin_src haskell
-- {-# LANGUAGE DataKinds, DeriveDataTypeable, GeneralizedNewtypeDeriving,
--              MultiParamTypeClasses, OverloadedStrings, TemplateHaskell,
--              TypeFamilies, TypeOperators #-}
-- module TutorialUsers where
-- import Control.Monad (mzero)
-- import Data.Readable (Readable(fromText))
-- import Data.Typeable
-- import qualified Data.Vector as V
-- import Frames.InCore (VectorFor)
-- import Frames

-- data GenderT = Male | Female deriving (Enum,Eq,Ord,Show,Typeable)

-- -- See @demo/TutorialUsers.hs@ in the repository for an example of how
-- -- to define a packed representation for a custom column type. It uses
-- -- the usual "Data.Vector.Generic" machinery.
-- type instance VectorFor GenderT = V.Vector

-- instance Readable GenderT where
--   fromText "M" = return Male
--   fromText "F" = return Female
--   fromText _ = mzero

-- type Columns' = GenderT ': CommonColumns
-- #+end_src

-- ** Splice Dump
-- The Template Haskell splices we use produce quite a lot of
-- code. The raw dumps of these splices can be hard to read, but I have
-- included some elisp code for cleaning up that output in the design
-- notes for =Frames=. Here is what we get from the ~tableTypesOpt~
-- splice shown above.

-- The overall structure is this:

-- - A ~Rec~ type called ~Users~ with all necessary columns
-- - A ~usersParser~ value that overrides parsing defaults
-- - A type synonym for each column that pairs the column name with its
--   type
-- - A lens to work with each column on any given row

-- Remember that for CSV files that include a header, the splice you
-- write in your code need not include the column names or separator
-- character.

-- #+BEGIN_EXAMPLE
--     tableTypesOpt
--       ["user id", "age", "gender", "occupation", "zip code"]
--       "|"
--       "Users"
--       "data/ml-100k/u.user"
--   ======>
--     type Users =
--         Rec ["user id" :-> Int, "age" :-> Int, "gender" :-> Text, "occupation" :-> Text, "zip code" :-> Text]

--     usersParser :: ParserOptions
--     usersParser
--       = ParserOptions
--           (Just
--              (map
--                 T.pack
--                 ["user id", "age", "gender", "occupation", "zip code"]))
--           (T.pack "|")

--     type UserId = "user id" :-> Int

--     userId ::
--       forall f_acWF rs_acWG. (Functor f_acWF,
--                               RElem UserId rs_acWG (Data.Vinyl.TypeLevel.RIndex UserId rs_acWG)) =>
--       (Int -> f_acWF Int) -> Rec rs_acWG -> f_acWF (Rec rs_acWG)
--     userId = rlens (Proxy :: Proxy UserId)

--     userId' ::
--       forall g_acWH f_acWI rs_acWJ. (Functor f_acWI,
--                                      Functor g_acWH,
--                                      RElem UserId rs_acWJ (Data.Vinyl.TypeLevel.RIndex UserId rs_acWJ)) =>
--       (g_acWH Int -> f_acWI (g_acWH Int))
--       -> RecF g_acWH rs_acWJ -> f_acWI (RecF g_acWH rs_acWJ)
--     userId' = rlens' (Proxy :: Proxy UserId)

--     type Age = "age" :-> Int

--     age ::
--       forall f_acWK rs_acWL. (Functor f_acWK,
--                               RElem Age rs_acWL (Data.Vinyl.TypeLevel.RIndex Age rs_acWL)) =>
--       (Int -> f_acWK Int) -> Rec rs_acWL -> f_acWK (Rec rs_acWL)
--     age = rlens (Proxy :: Proxy Age)

--     age' ::
--       forall g_acWM f_acWN rs_acWO. (Functor f_acWN,
--                                      Functor g_acWM,
--                                      RElem Age rs_acWO (Data.Vinyl.TypeLevel.RIndex Age rs_acWO)) =>
--       (g_acWM Int -> f_acWN (g_acWM Int))
--       -> RecF g_acWM rs_acWO -> f_acWN (RecF g_acWM rs_acWO)
--     age' = rlens' (Proxy :: Proxy Age)

--     type Gender = "gender" :-> Text

--     gender ::
--       forall f_acWP rs_acWQ. (Functor f_acWP,
--                               RElem Gender rs_acWQ (Data.Vinyl.TypeLevel.RIndex Gender rs_acWQ)) =>
--       (Text -> f_acWP Text) -> Rec rs_acWQ -> f_acWP (Rec rs_acWQ)
--     gender = rlens (Proxy :: Proxy Gender)

--     gender' ::
--       forall g_acWR f_acWS rs_acWT. (Functor f_acWS,
--                                      Functor g_acWR,
--                                      RElem Gender rs_acWT (Data.Vinyl.TypeLevel.RIndex Gender rs_acWT)) =>
--       (g_acWR Text -> f_acWS (g_acWR Text))
--       -> RecF g_acWR rs_acWT -> f_acWS (RecF g_acWR rs_acWT)
--     gender' = rlens' (Proxy :: Proxy Gender)

--     type Occupation = "occupation" :-> Text

--     occupation ::
--       forall f_acWU rs_acWV. (Functor f_acWU,
--                               RElem Occupation rs_acWV (Data.Vinyl.TypeLevel.RIndex Occupation rs_acWV)) =>
--       (Text -> f_acWU Text) -> Rec rs_acWV -> f_acWU (Rec rs_acWV)
--     occupation = rlens (Proxy :: Proxy Occupation)

--     occupation' ::
--       forall g_acWW f_acWX rs_acWY. (Functor f_acWX,
--                                      Functor g_acWW,
--                                      RElem Occupation rs_acWY (Data.Vinyl.TypeLevel.RIndex Occupation rs_acWY)) =>
--       (g_acWW Text -> f_acWX (g_acWW Text))
--       -> RecF g_acWW rs_acWY -> f_acWX (RecF g_acWW rs_acWY)
--     occupation' = rlens' (Proxy :: Proxy Occupation)

--     type ZipCode = "zip code" :-> Text

--     zipCode ::
--       forall f_acWZ rs_acX0. (Functor f_acWZ,
--                               RElem ZipCode rs_acX0 (Data.Vinyl.TypeLevel.RIndex ZipCode rs_acX0)) =>
--       (Text -> f_acWZ Text) -> Rec rs_acX0 -> f_acWZ (Rec rs_acX0)
--     zipCode = rlens (Proxy :: Proxy ZipCode)

--     zipCode' ::
--       forall g_acX1 f_acX2 rs_acX3. (Functor f_acX2,
--                                      Functor g_acX1,
--                                      RElem ZipCode rs_acX3 (Data.Vinyl.TypeLevel.RIndex ZipCode rs_acX3)) =>
--       (g_acX1 Text -> f_acX2 (g_acX1 Text))
--       -> RecF g_acX1 rs_acX3 -> f_acX2 (RecF g_acX1 rs_acX3)
--     zipCode' = rlens' (Proxy :: Proxy ZipCode)
-- #+END_EXAMPLE

-- ** Thanks
-- Thanks to Greg Hale for reviewing an early draft of this document.

-- #+DATE:
-- #+TITLE: Frames Tutorial
-- #+OPTIONS: html-link-use-abs-url:nil html-postamble:nil
