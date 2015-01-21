-- | Functions useful for interactively exploring and experimenting
-- with a data set.
module Frames.Exploration where
import Pipes
import Pipes.Prelude as P

-- | @preview src n f@ prints out the first @n@ results of piping
-- @src@ through @f@.
pipePreview :: (MonadIO m, Show b)
            => Producer a m () -> Int -> Pipe a b m () -> m ()
pipePreview src n f = runEffect $ src >-> f >-> P.take n >-> P.print
