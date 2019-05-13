module Main (nucleotideCounts, nucleotideCounts2, main) where

import Data.Map (Map)
import qualified Data.Map as Map
import Criterion.Main

nucleotideCounts :: String -> Either String (Map Char Int)
nucleotideCounts xs
    | length xs /= length valid_nucleotides = Left ("no és una seqüència vàlida")
    | otherwise = Right $ foldl (\map k -> Map.insertWith (+) k 1 map) starting_map valid_nucleotides
    where
        nucleotides = ['A', 'C', 'G', 'T']
        valid_nucleotides = filter (\x -> elem x nucleotides) xs
        starting_map = Map.fromList (map (\x -> (x, 0)) nucleotides)


nucleotideCount :: Char -> Map Char Int -> Map Char Int
nucleotideCount x = Map.insertWith (+) x 1

defaultMap :: Map Char Int
defaultMap = Map.fromList $ zip ['A', 'C', 'G', 'T'] (repeat 0)

nucleotideCounts' :: String -> Map Char Int
nucleotideCounts' = foldr nucleotideCount defaultMap

validateStrand :: Map Char Int -> Map Char Int -> Bool
validateStrand = fmap and . sequence . map Map.member . Map.keys

validStrand :: Map Char Int -> Bool
validStrand = flip validateStrand defaultMap

nucleotideCounts2 :: String -> Either String (Map Char Int)
nucleotideCounts2 s
  | validStrand strand = Right strand
  | otherwise = Left "invalid strand"
  where strand = nucleotideCounts' s

main = defaultMain [
  bgroup "long" [ bench "submission" $ whnf nucleotideCounts "AGCTTTTCATTCTGACTGCAACGGGCAATATGTCTCTGTGTGGATTAAAAAAAGAGTGTCTGATAGCAGC"
                       , bench "mine" $ whnf nucleotideCounts2 "AGCTTTTCATTCTGACTGCAACGGGCAATATGTCTCTGTGTGGATTAAAAAAAGAGTGTCTGATAGCAGC"
                       ],
  bgroup "really long" [ bench "submission" $ whnf nucleotideCounts "AGCTTTTCATTCTGACTGCAACGGGCAATATGTCTCTGTGTGGATTAAAAAAAGAGTGTCTGATAGCAGCAGCTTTTCATTCTGACTGCAACGGGCAATATGTCTCTGTGTGGATTAAAAAAAGAGTGTCTGATAGCAGCAGCTTTTCATTCTGACTGCAACGGGCAATATGTCTCTGTGTGGATTAAAAAAAGAGTGTCTGATAGCAGC"
                       , bench "mine" $ whnf nucleotideCounts2 "AGCTTTTCATTCTGACTGCAACGGGCAATATGTCTCTGTGTGGATTAAAAAAAGAGTGTCTGATAGCAGCAGCTTTTCATTCTGACTGCAACGGGCAATATGTCTCTGTGTGGATTAAAAAAAGAGTGTCTGATAGCAGCAGCTTTTCATTCTGACTGCAACGGGCAATATGTCTCTGTGTGGATTAAAAAAAGAGTGTCTGATAGCAGC"
                       ],
  bgroup "empty" [ bench "submission" $ whnf nucleotideCounts ""
                       , bench "mine" $ whnf nucleotideCounts2 ""
                       ],
  bgroup "singleton" [ bench "submission" $ whnf nucleotideCounts "G"
                       , bench "mine" $ whnf nucleotideCounts2 "G"
                       ],
  bgroup "repetitive" [ bench "submission" $ whnf nucleotideCounts "GGGGGGGG"
                       , bench "mine" $ whnf nucleotideCounts2 "GGGGGGGG"
                       ],
  bgroup "invalid" [ bench "submission" $ whnf nucleotideCounts "AGXXACT"
                       , bench "mine" $ whnf nucleotideCounts2 "AGXXACT"
                       ],
  bgroup "long invalid" [ bench "submission" $ whnf nucleotideCounts "AGXXACTAGCTTTTCATTCTGACTGCAACGGGCAATATGTCTCTGTGTGGATTAAAAAAAGAGTGTCTGATAGCAGCAGCTTTTCATTCTGACTGCAACGGGCAATATGTCTCTGTGTGGATTAAAAAAAGAGTGTCTGATAGCAGCAGCTTTTCATTCTGACTGCAACGGGCAATATGTCTCTGTGTGGATTAAAAAAAGAGTGTCTGATAGCAGC"
                       , bench "mine" $ whnf nucleotideCounts2 "AGXXACTAGCTTTTCATTCTGACTGCAACGGGCAATATGTCTCTGTGTGGATTAAAAAAAGAGTGTCTGATAGCAGCAGCTTTTCATTCTGACTGCAACGGGCAATATGTCTCTGTGTGGATTAAAAAAAGAGTGTCTGATAGCAGCAGCTTTTCATTCTGACTGCAACGGGCAATATGTCTCTGTGTGGATTAAAAAAAGAGTGTCTGATAGCAGC"
                       ]
    ]
