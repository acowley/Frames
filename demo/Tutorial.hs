{-# LANGUAGE ConstraintKinds, DataKinds, FlexibleContexts, GADTs,
             OverloadedStrings, PatternSynonyms, QuasiQuotes,
             ScopedTypeVariables, TemplateHaskell, TypeApplications,
             TypeOperators, ViewPatterns #-}

-- This is a loose port of a
-- [[https://ajkl.github.io/2014/11/23/Dataframes/][dataframe tutorial]] Rosetta
-- Stone to compare traditional dataframe tools built in R, Julia,
-- Python, etc. with
-- [[https://github.com/acowley/Frames][Frames]]. Performing data
-- analysis in Haskell brings with it a few advantages:

-- - Interactive exploration is supported in GHCi
-- - GHC produces fast, memory-efficient code when you're ready to run a
--   program that might take a bit of time
-- - You get to use Haskell to write your own functions when what you
--   want isn't already defined in the library
-- - The code you write is /statically typed/ so that mismatches between
--   your code and your data data are found by the type checker

-- The example [[http://grouplens.org/datasets/movielens/][data]] file
-- used (specifically, the =u.user= file from the /MovieLens 100k/
-- data set) does not include column headers, nor does it use commas
-- to separate values, so it does not fall into the sweet spot of CSV
-- parsing that ~Frames~ is aimed at. That said, this mismatch of test
-- data and library support is a great opportunity to verify that
-- ~Frames~ are flexible enough to meet a variety of needs.

-- We begin with rather a lot of imports to support a variety of test
-- operations and parser customization. I encourage you to start with a
-- smaller test program than this!

import qualified Control.Foldl as L
import qualified Data.Foldable as F
import Data.Vinyl
import Data.Vinyl.Functor (Identity(..), Const(..))
import Lens.Micro
import Lens.Micro.Extras
import Frames
import Frames.CSV (readTableOpt, rowGen, RowGen(..))
import Pipes hiding (Proxy)
import qualified Pipes.Prelude as P

-- A few other imports will be used for highly customized parsing [[Better Types][later]].

import Frames.CSV (colQ)
import TutorialZipCode

-- * Data Import

-- We usually package column names with the data to keep things a bit
-- more self-documenting. In the common case where a data file has a
-- header row providing column names, and columns are separated by
-- commas, generating the types needed to import a data set is as simple
-- as,

-- -- #+BEGIN_SRC haskell
-- tableTypes "User" "data/ml-100k/u.user"
-- #+END_SRC

-- The data set /this/ example considers is rather far from the sweet
-- spot of CSV processing that ~Frames~ is aimed it: it does not include
-- column headers, nor does it use commas to separate values!  However,
-- these mismatches do provide an opportunity to see that the ~Frames~
-- library is flexible enough to meet a variety of needs.

tableTypes' (rowGen "data/ml-100k/u.user")
            { rowTypeName = "User"
            , columnNames = [ "user id", "age", "gender"
                            , "occupation", "zip code" ]
            , separator = "|" }

-- This template haskell splice explicitly specifies the name for the
-- inferred record type, column names, a separator string, and the
-- data file from which to infer the record type (i.e. what type
-- should be used to represent each column). The result of this splice
-- is included in an [[* Splice Dump][appendix]] below so you can flip
-- between the generated code and how it is used.

-- Since this data is far from the ideal CSV file, we have to tell
-- ~Frames~ how to interpret the data so that it can decide what data
-- type to use for each column. Having the types depend upon the data in
-- the given file is a useful exercise in this domain as the actual shape
-- of the data is of paramount importance during the early import and
-- exploration phases of data analysis.

-- We can load the module into =cabal repl= to see what we have so far.

-- #+BEGIN_EXAMPLE
-- λ> :i User
-- type User =
--   Record
--     '["user id" :-> Int, "age" :-> Int, "gender" :-> Text,
--       "occupation" :-> Text, "zip code" :-> Text]
-- #+END_EXAMPLE

-- This lets us perform a quick check that the types are basically what
-- we expect them to be.

-- We now define a streaming representation of the full data set. If the
-- data set is too large to keep in memory, we can process it as it
-- streams through RAM.

movieStream :: MonadSafe m => Producer User m ()
movieStream = readTableOpt userParser "data/ml-100k/u.user"

-- Alternately, if we want to run multiple operations against a data set
-- that /can/ fit in RAM, we can do that. Here we define an in-core (in
-- memory) array of structures (AoS) representation.

loadMovies :: IO (Frame User)
loadMovies = inCoreAoS movieStream

-- ** Streaming Cores?

-- A ~Frame~ is an in-memory representation of your data. The ~Frames~
-- library stores each column as compactly as it knows how, and lets
-- you index your data as a structure of arrays (where each field of
-- the structure is an array corresponding to a column of your data),
-- or as an array of structures, also known as a ~Frame~. These latter
-- structures correspond to rows of your data. Alternatively, rows of
-- data may be handled in a streaming fashion so that you are not
-- limited to available RAM. In the streaming paradigm, you process
-- each row individually as a single record.

-- A ~Frame~ provides ~O(1)~ indexing, as well as any other operations
-- you are familiar with based on the ~Foldable~ class. If a data set is
-- small, keeping it in RAM is usually the fastest way to perform
-- multiple analyses on that data that you can't fuse into a single
-- traversal.

-- Alternatively, a ~Producer~ of rows is a great way to whittle down a
-- large data set before moving on to whatever you want to do next.

-- The upshot is that you can work with your data as a collection of
-- rows with either a densely packed in-memory reporesentation -- a
-- ~Frame~ -- or a stream of rows provided by a ~Producer~. The choice
-- depends on if you want to perform multiple queries against your
-- data, and, if so, whether you have enough RAM to hold the data. If
-- the answer to both of those questions is,
-- @@html:<i>@@"Yes!"@@html:</i>@@, consider using a ~Frame~ as in the
-- ~loadMovies~ example. If the answer to either question is,
-- @@html:<i>@@"Nope!"@@html:</i>@@, you will be better off with a
-- ~Producer~, as in the ~movieStream~ example.

-- ** Sanity Check

-- We can compute some easy statistics to see how things look.

-- #+BEGIN_EXAMPLE
-- λ> ms <- loadMovies
-- λ> L.fold L.minimum (view age <$> ms)
-- Just 7
-- #+END_EXAMPLE

-- When there are multiple properties we would like to compute, we can
-- fuse multiple traversals into one pass using something like the [[http://hackage.haskell.org/package/foldl][foldl]]
-- package,

minMax :: Ord a => L.Fold a (Maybe a, Maybe a)
minMax = (,) <$> L.minimum <*> L.maximum

-- #+BEGIN_EXAMPLE
-- λ> L.fold (L.handles age minMax) ms
-- (Just 7,Just 73)
-- #+END_EXAMPLE

-- Here we are projecting the =age= column out of each record, and
-- computing the minimum and maximum =age= across all rows.

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
-- λ> take 6 . F.toList $ view occupation <$> ms
-- ["technician","other","writer","technician","other","executive"]
-- #+END_EXAMPLE

-- Or multiple columns,

miniUser :: User -> Record '[Occupation, Gender, Age]
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

-- If you'd rather not define a function like ~miniUser~, you can fix
-- the types in-line by using the ~rcast~ function.

-- #+BEGIN_EXAMPLE
-- λ> :set -XTypeApplications -XDataKinds
-- λ> rcast @'[Occupation,Gender,Age] $ frameRow ms 0
-- {occupation :-> "technician", gender :-> "M", age :-> 24}
-- #+END_EXAMPLE

-- ** Query / Conditional Subset

-- Filtering our frame is rather nicely done using the
-- [[http://hackage.haskell.org/package/pipes][pipes]] package. Here
-- we pick out the users whose occupation is "writer".

writers :: (Occupation ∈ rs, Monad m) => Pipe (Record rs) (Record rs) m r
writers = P.filter ((== "writer") . view occupation)

-- #+BEGIN_EXAMPLE
-- λ> runSafeEffect $ movieStream >-> writers >-> P.take 6 >-> P.print
-- {user id :-> 3, age :-> 23, gender :-> "M", occupation :-> "writer", zip code :-> "32067"}
-- {user id :-> 21, age :-> 26, gender :-> "M", occupation :-> "writer", zip code :-> "30068"}
-- {user id :-> 22, age :-> 25, gender :-> "M", occupation :-> "writer", zip code :-> "40206"}
-- {user id :-> 28, age :-> 32, gender :-> "M", occupation :-> "writer", zip code :-> "55369"}
-- {user id :-> 50, age :-> 21, gender :-> "M", occupation :-> "writer", zip code :-> "52245"}
-- {user id :-> 122, age :-> 32, gender :-> "F", occupation :-> "writer", zip code :-> "22206"}
-- #+END_EXAMPLE

-- If you're not too keen on all the ~pipes~ syntax in that example, you
-- could also write it using a helper function provided by ~Frames~,

-- #+BEGIN_EXAMPLE
-- λ> pipePreview movieStream 6 writers
-- #+END_EXAMPLE

-- This is a handy way to try out various maps and filters you may want
-- to eventually apply to a large data set.

-- ** Column Subset Update

-- We can also apply a function to a subset of columns of each row! Here,
-- we want to apply a function with type ~Int -> Int~ to two columns
-- whose values are of type ~Int~.

intFieldDoubler :: Record '[UserId, Age] -> Record '[UserId, Age]
intFieldDoubler = mapMono (* 2)

-- Let's preview the effect of this function by applying it to the
-- ~UserId~ and ~Age~ columns of the first three rows of our data set.

-- #+BEGIN_EXAMPLE
-- λ> pipePreview movieStream 3 (P.map (rsubset %~ intFieldDoubler))
-- {user id :-> 2, age :-> 48, gender :-> "M", occupation :-> "technician", zip code :-> "85711"}
-- {user id :-> 4, age :-> 106, gender :-> "F", occupation :-> "other", zip code :-> "94043"}
-- {user id :-> 6, age :-> 46, gender :-> "M", occupation :-> "writer", zip code :-> "32067"}
-- #+END_EXAMPLE

-- This is a neat way of manipulating a few columns without having to
-- worry about what other columns might exist. You might want to use this
-- for normalizing the capitalization, or truncating the length of,
-- various text fields, for example.

-- ** Mostly-Uniform Data

-- /(Warning: This section veers into types that are likely of more
-- use to library authors than end users.)/

-- Suppose we don't know much about our data, but we do know that it
-- starts with an identifying column, and then some number of numeric
-- columns. We can structurally peel off the first column, perform a
-- constrained polymorphic operation on the other columns, then glue
-- the first column back on to the result.

addTwoRest :: RecMapMethod Num ElField rs
           => Record (s :-> a ': rs) -> Record (s :-> a ': rs)
addTwoRest (h :& t) = h :& aux t
  where aux = mapFields @Num (\x -> x + 2)

-- #+BEGIN_EXAMPLE
-- λ> addTwoRest (rcast @'[Occupation, UserId, Age] (frameRow ms 0))
-- {occupation :-> "technician", user id :-> 3, age :-> 26}
-- #+END_EXAMPLE

-- But what if we don't want to rely entirely on ordering of our rows?
-- Here, we know there is an identifying column, ~Occupation~, and we
-- want to shuffle it around to the head of the record while mapping a
-- constrained polymorphic operation over the other columns.

addTwoOccupation :: (CanDelete Occupation rs,
                     rs' ~ RDelete Occupation rs,
                     RecMapMethod Num ElField rs')
                 => Record rs -> Record (Occupation ': RDelete Occupation rs)
addTwoOccupation r =
  rget @Occupation r :& mapFields @Num (+ 2) (rdel @Occupation r)

-- #+BEGIN_EXAMPLE
-- λ> addTwoOccupation (rcast @'[UserId,Age,Occupation] (frameRow ms 0))
-- {occupation :-> "technician", user id :-> 3, age :-> 26}
-- #+END_EXAMPLE

-- It is a bit clumsy to delete and then add back a particular field,
-- and the dependence on explicit structure is relying a bit more on
-- coincidence than we might like. We could choose, instead, to work
-- with row types that contain a distinguished column somewhere in
-- their midst, but regarding precisely /where/ it is, or /how many/
-- other fields there are, we care not.

addTwoOccupation' :: forall rs rs'.
                     (CanDelete Occupation rs,
                      rs' ~ RDelete Occupation rs,
                      RecMapMethod Num ElField rs')
                  => Record rs -> Record rs
addTwoOccupation' = rsubset %~ mapFields @Num @rs' (+ 2)

-- #+BEGIN_EXAMPLE
-- λ> addTwoOccupation' (rcast @'[UserId,Age,Occupation] (frameRow ms 0))
-- {user id :-> 3, age :-> 26, occupation :-> "technician"}
-- #+END_EXAMPLE

-- We can unpack this type a bit to understand what is happening. A
-- ~Frames~ ~Record~ is a record from the ~Vinyl~ library, except that
-- each type has phantom column information. This metadata is
-- available to the type checker, but is erased during compilation so
-- that it does not impose any runtime overhead. What we are doing
-- here is saying that we will operate on a ~Frames~ row type, ~Record
-- rs~, that has an element ~Occupation~, and that deleting this
-- element works properly (i.e. the leftover fields are a proper
-- subset of the original row type). We further state -- with the
-- ~AsVinyl~ constraint -- that we want to work on the unadorned field
-- values, temporarily discarding their header information, with the
-- ~mapMethod~ function that will treat our richly-typed row as a less
-- informative ~Vinyl~ record.

-- We then peer through a lens onto the set of all unadorned fields other
-- than ~Occupation~, apply a function with a ~Num~ constraint to each of
-- those fields, then pull back out of the lens reattaching the column
-- header information on our way. All of that manipulation and
-- bookkeeping is managed by the type checker.

-- Lest we forget we are working in a typed setting, what happens if
-- the constraint on our polymorphic operation can't be satisfied by
-- one of the columns?

-- #+BEGIN_EXAMPLE
-- λ> addTwoOccupation (rcast @'[Age,Occupation,Gender] (frameRow ms 0))

-- <interactive>:15:1-16:
--     No instance for (Num Text) arising from a use of ‘addTwoOccupation’
--     In the expression:
--       addTwoOccupation
--         (rcast @'[Age, Occupation, Gender] (frameRow ms 0))
--     In an equation for ‘it’:
--         it
--           = addTwoOccupation
--               (rcast @'[Age, Occupation, Gender] (frameRow ms 0))
-- #+END_EXAMPLE

-- This error message isn't ideal in that it doesn't tell us which
-- column failed to satisfy the constraint. Hopefully this can be
-- improved in the future!

-- * Escape Hatch

-- When you're done with ~Frames~ and want to get back to more
-- familiar monomorphic pastures, you can bundle your data up.

restInts :: (RecordToList (Unlabeled rs), StripFieldNames rs,
             RecMapMethod ((~) Int) Identity (Unlabeled rs))
         => Record (s :-> Text ': rs) -> (Text, [Int])
restInts (recUncons -> (h,t)) = (h, recordToList t')
  where t' = rmapMethod @((~) Int) (Const . getIdentity) (stripNames t)

-- #+BEGIN_EXAMPLE
-- λ> restInts (rcast @'[Occupation,UserId,Age] (frameRow ms 0))
-- ("technician",[1,24])
-- #+END_EXAMPLE

-- * Better Types

-- A common disappointment of parsing general data sets is the
-- reliance on text for data representation even /after/ parsing. If
-- you find that the default ~Columns~ spectrum of potential column
-- types that =Frames= uses doesn't capture desired structure, you can
-- go ahead and define your own universe of column types! The =User=
-- row types we've been playing with here is rather boring: it only
-- uses =Int= and =Text= column types. But =Text= is far too vague a
-- type for a column like =zipCode=.

-- All of the zip codes in this set are five characters, and most are
-- standard numeric US zip codes. Let's go ahead and define our own
-- universe of column types.

-- -- #+begin_src haskell
-- data ZipT = ZipUS Int Int Int Int Int
--           | ZipWorld Char Char Char Char Char
--   deriving (Eq, Ord, Show, Typeable)

-- type instance VectorFor ZipT = V.Vector

-- instance Readable ZipT  where
--   fromText t
--       | T.length t == 5 = let cs@[v,w,x,y,z] = T.unpack t
--                               [a,b,c,d,e] = map C.digitToInt cs
--                           in if all C.isDigit cs
--                              then return $ ZipUS a b c d e
--                              else return $ ZipWorld v w x y z
--       | otherwise = mzero

-- type MyColumns = ZipT ': CommonColumns
-- #+end_src

-- Note that these definitions must be imported from a separate module
-- to satisfy GHC's stage restrictions related to Template
-- Haskell. The full code for the custom type may be found in an [[*
-- User Types][appendix]].

-- We name this record type ~U2~, and give all the generated column types
-- and lenses a prefix, "u2", so they don't conflict with the definitions
-- we generated earlier.

tableTypes' (rowGen "data/ml-100k/u.user")
            { rowTypeName = "U2"
            , columnNames = [ "user id", "age", "gender"
                            , "occupation", "zip code" ]
            , separator = "|"
            , tablePrefix = "u2"
            , columnUniverse = $(colQ ''MyColumns) }


movieStream2 :: MonadSafe m => Producer U2 m ()
movieStream2 = readTableOpt u2Parser "data/ml-100k/u.user"

-- This new record type, =U2=, has a more interesting =zip code=
-- column.

-- #+BEGIN_EXAMPLE
-- λ> :i U2
-- type U2 =
--   Record
--     '["user id" :-> Int, "age" :-> Int, "gender" :-> Text,
--       "occupation" :-> Text, "zip code" :-> ZipT]
-- #+END_EXAMPLE

-- Let's take the occupations of the first 10 users from New England,
-- New Jersey, and other places whose zip codes begin with a zero.

neOccupations :: (U2ZipCode ∈ rs, U2Occupation ∈ rs, Monad m)
              => Pipe (Record rs) Text m r
neOccupations = P.filter (isNewEngland . view u2ZipCode)
                >-> P.map (view u2Occupation)
  where isNewEngland (ZipUS 0 _ _ _ _) = True
        isNewEngland _ = False

-- #+BEGIN_EXAMPLE
-- λ> runSafeEffect $ movieStream2 >-> neOccupations >-> P.take 10 >-> P.print
-- "administrator"
-- "student"
-- "other"
-- "programmer"
-- "librarian"
-- "entertainment"
-- "marketing"
-- "programmer"
-- "educator"
-- "healthcare"
-- #+END_EXAMPLE

-- So there we go! We've done both row and column subset queries with a
-- strongly typed query (namely, ~isNewEngland~). Another situation in
-- which one might want to define a custom universe of column types is
-- when dealing with dates. This would let you both reject rows with
-- badly formatted dates, for example, and efficiently query the data set
-- with richly-typed queries.

-- Even better, did you notice the types of ~writers~ and
-- ~neOccupations~? They are polymorphic over the full row type!
-- That's what the ~(Occupation ∈ rs)~ constraint signifies: such a
-- function will work for record types with any set of fields, ~rs~, so
-- long as ~Occupation~ is an element of that set. This means that if
-- your schema changes, or you switch to a related but different data
-- set, *these functions can still be used without even touching the
-- code*. Just recompile against the new data set, and you're good to go.

-- * Appendix
-- ** User Types

-- Here are the definitions needed to define the ~MyColumns~ type with
-- its more descriptive ~ZipT~ type. We have to define these things in
-- a separate module from our main work due to GHC's stage
-- restrictions regarding Template Haskell. Specifically, ~ZipT~ and
-- its instances are used at compile time to infer the record type
-- needed to represent the data file. Notice the extension point here
-- is not too rough: you prepend new, more refined, type compatibility
-- checks to the head of ~CommonColumns~, or you can build up your own
-- list of expected types.

-- This may not be something you'd want to do for every data
-- set. However, the /ability/ to refine the structure of parsed data
-- is in keeping with the overall goal of ~Frames~: it's easy to take
-- off, and the sky's the limit.

-- -- #+begin_src haskell
-- {-# LANGUAGE DataKinds, DeriveDataTypeable, TypeFamilies, TypeOperators #-}
-- module TutorialZipCode where
-- import Control.Monad (mzero)
-- import qualified Data.Char as C
-- import Data.Readable (Readable(fromText))
-- import qualified Data.Text as T
-- import Data.Typeable
-- import qualified Data.Vector as V
-- import Frames.InCore (VectorFor)
-- import Frames

-- data ZipT = ZipUS Int Int Int Int Int
--           | ZipWorld Char Char Char Char Char
--   deriving (Eq, Ord, Show, Typeable)

-- type instance VectorFor ZipT = V.Vector

-- instance Readable ZipT  where
--   fromText t
--       | T.length t == 5 = let cs@[v,w,x,y,z] = T.unpack t
--                               [a,b,c,d,e] = map C.digitToInt cs
--                           in if all C.isDigit cs
--                              then return $ ZipUS a b c d e
--                              else return $ ZipWorld v w x y z
--       | otherwise = mzero

-- type MyColumns = ZipT ': CommonColumns
-- #+end_src

-- ** Splice Dump

-- The Template Haskell splices we use produce quite a lot of
-- code. The raw dumps of these splices can be hard to read, but I
-- have included some elisp code for cleaning up that output in the
-- design notes for =Frames=. Here is what we get from the
-- @@html:<code>@@tableTypes'@@html:</code>@@ splice shown above.

-- The overall structure is this:

-- - A ~Record~ type called ~User~ with all necessary columns
-- - A ~userParser~ value that overrides parsing defaults
-- - A type synonym for each column that pairs the column name with its
--   type
-- - A lens to work with each column on any given row

-- Remember that for CSV files that include a header, the splice you
-- write in your code need not include the column names or separator
-- character.

-- -- #+begin_src haskell
--   tableTypes'
--     (rowGen "data/ml-100k/u.user")
--     {rowTypeName = "User",
--      columnNames = ["user id", "age", "gender", "occupation",
--                     "zip code"],
--      separator = "|"})
-- ======>
-- type User = Record ["user id" :-> Int, "age" :-> Int, "gender" :-> Text, "occupation" :-> Text, "zip code" :-> Text]

-- userParser :: ParserOptions
-- userParser = ParserOptions (Just (map T.pack ["user id"
--                                              , "age"
--                                              , "gender"
--                                              , "occupation"
--                                              , "zip code"]))
--                            (T.pack "|")
--                            (Frames.CSV.RFC4180Quoting '"')

-- type UserId = "user id" :-> Int
-- userId :: forall f_0 rs_1 .
--           (Functor f_0, RElem UserId rs_1 (RIndex UserId rs_1))
--        => (Int -> f_0 Int)
--        -> Record rs_1
--        -> f_0 (Record rs_1)
-- userId = rlens @UserId . rfield
-- userId' :: forall f_2 g_3 rs_4 .
--            (Functor f_2, RElem UserId rs_4 (RIndex UserId rs_4))
--         => (g_3 UserId -> f_2 (g_3 UserId))
--         -> Rec g_3 rs_4
--         -> f_2 (Rec g_3 rs_4)
-- userId' = rlens' @UserId

-- type Age = "age" :-> Int
-- age :: forall f_5 rs_6 .
--        (Functor f_5, RElem Age rs_6 (RIndex Age rs_6))
--     => (Int -> f_5 Int)
--     -> Record rs_6
--     -> f_5 (Record rs_6)
-- age = rlens @Age . rfield
-- age' :: forall f_7 g_8 rs_9 .
--         (Functor f_7, RElem Age rs_9 (RIndex Age rs_9))
--      => (g_8 Age -> f_7 (g_8 Age))
--      -> Rec g_8 rs_9
--      -> f_7 (Rec g_8 rs_9)
-- age' = rlens' @Age

-- type Gender = "gender" :-> Text
-- gender :: forall f_10 rs_11 .
--           (Functor f_10, RElem Gender rs_11 (RIndex Gender rs_11))
--        => (Text -> f_10 Text)
--        -> Record rs_11
--        -> f_10 (Record rs_11)
-- gender = rlens @Gender . rfield
-- gender' :: forall f_12 g_13 rs_14 .
--            (Functor f_12, RElem Gender rs_14 (RIndex Gender rs_14))
--         => (g_13 Gender -> f_12 (g_13 Gender))
--         -> Rec g_13 rs_14
--         -> f_12 (Rec g_13 rs_14)
-- gender' = rlens' @Gender

-- type Occupation = (:->) "occupation"
--                                    Text
-- occupation :: forall f_15 rs_16 .
--               (Functor f_15, RElem Occupation rs_16 (RIndex Occupation rs_16))
--            => (Text -> f_15 Text)
--            -> Record rs_16
--            -> f_15 (Record rs_16)
-- occupation = rlens @Occupation . rfield
-- occupation' :: forall f_17 g_18 rs_19 .
--                (Functor f_17, RElem Occupation rs_19 (RIndex Occupation rs_19))
--             => (g_18 Occupation -> f_17 (g_18 Occupation))
--             -> Rec g_18 rs_19
--             -> f_17 (Rec g_18 rs_19)
-- occupation' = rlens' @Occupation

-- type ZipCode = "zip code" :-> Text
-- zipCode :: forall f_20 rs_21 .
--            (Functor f_20, RElem ZipCode rs_21 (RIndex ZipCode rs_21))
--         => (Text -> f_20 Text)
--         -> Record rs_21
--         -> f_20 (Record rs_21)
-- zipCode = rlens @ZipCode . rfield
-- zipCode' :: forall f_22 g_23 rs_24 .
--             (Functor f_22, RElem ZipCode rs_24 (RIndex ZipCode rs_24))
--          => (g_23 ZipCode -> f_22 (g_23 ZipCode))
--          -> Rec g_23 rs_24
--          -> f_22 (Rec g_23 rs_24)
-- zipCode' = rlens' @ZipCode
-- #+end_src

-- ** Thanks
-- Thanks to Greg Hale and Ben Gamari for reviewing early drafts of this document.

-- #+DATE:
-- #+TITLE: Frames Tutorial
-- #+OPTIONS: html-link-use-abs-url:nil html-postamble:nil
