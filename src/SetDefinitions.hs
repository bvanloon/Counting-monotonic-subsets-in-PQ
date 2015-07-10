
module SetDefinitions where
import Data.Bits

{- The pairs of elements for the fragment containing two atoms -}
negativeSet :: [Int]
negativeSet = [0..14]

positiveSet :: [Int]
positiveSet = [255,238,205,206,174,171,204,170]

totalSet :: [Int]
totalSet = map fromIntegral (negativeSet ++  positiveSet)