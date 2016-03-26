import qualified Data.Char as C
import qualified Data.Map as M
import           Data.Maybe (mapMaybe)
import qualified Data.Set as S

{-| This program outputs all ham radio callsigns that are leetspeak words. The
output looks like the following:

("WAX","W4X")
("WAXED","W4XED")
("WAXEN","W4XEN")
("WAXES","W4XES")
("WAXS","W4XS")
("WAXY","W4XY")
("WAY","W4Y")
("WAYNE","W4YNE")
("WAYS","W4YS")
("WEAK","WE4K")

-}
main :: IO ()
main = do
    dictionary <-
        pure . S.fromList . lines . map C.toUpper . filter (/= '\'') =<<
        readFile "/usr/share/dict/american-english"
    let alpha = ['A' .. 'Z']
        region = ['0', '1', '3', '4']
        oneByOnes =
            [[a, n, b] | a <- "KNW"
                       , n <- region
                       , b <- alpha]
        oneByTwos =
            [a ++ [b] | a <- oneByOnes
                      , b <- alpha]
        oneByThrees =
            [a ++ [b] | a <- oneByTwos
                      , b <- alpha]
        twoByOnes =
            [[a, b, n, c] | a <- "AKNW"
                          , b <- alpha
                          , n <- region
                          , b `S.notMember`
                                if a == 'A'
                                    then S.fromList "HLMNOPQRSTUVWXYZ"
                                    else S.fromList "HLP"
                          , c <- alpha]
        twoByTwos =
            [a ++ [b] | a <- twoByOnes
                      , b <- alpha]
        twoByThrees =
            [a ++ [b] | a <- twoByTwos
                      , b <- alpha]
        calls =
            concat
                [ oneByOnes
                , oneByTwos
                , oneByThrees
                , twoByOnes
                , twoByTwos
                , twoByThrees]
        leet '0' = 'O'
        leet '1' = 'I'
        leet '3' = 'E'
        leet '4' = 'A'
        leet x = x
        callWords = map (map leet) calls
        wordToCall = M.fromList (zip callWords calls)
        wordMatches = S.intersection (S.fromList callWords) dictionary
        matchingPairs =
            mapMaybe
                (\word ->
                      fmap ((,) word) (M.lookup word wordToCall))
                (S.toList wordMatches)
    mapM_ print matchingPairs
