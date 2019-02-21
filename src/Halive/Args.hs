{-# LANGUAGE RecordWildCards #-}

module Halive.Args
    ( Args(..)
    , FileType
    , parseArgs
    , usage) where

type FileType = String

data Args = Args
    { mainFileName  :: String
    , includeDirs   :: [String]
    , fileTypes     :: [FileType]
    , targetArgs    :: [String]
    , shouldCompile :: Bool
    }

data PartialArgs = PartialArgs
    { mainFileName'  :: Maybe String
    , includeDirs'   :: [String]
    , fileTypes'     :: [FileType]
    , targetArgs'    :: [String]
    , shouldCompile' :: Bool
    }

usage :: String
usage = "Usage: halive <main.hs> [<include dir>] [-f|--file-type <file type>] [-- <args to myapp>]\n\
        \\n\
        \Available options:\n\
        \  -f, --file-type <file type>     Custom file type to watch for changes (e.g. \"-f html\")\n\
        \  -c, --compiled                  Faster code (but slower compilation)"


parseArgs :: [String] -> Maybe Args
parseArgs args = go args (PartialArgs Nothing [] [] [] False) >>= fromPartial
    where
        go :: [String] -> PartialArgs -> Maybe PartialArgs
        go [] partial = Just partial
        go (x : xs) partial
            | x == "--" = Just partial { targetArgs' = xs }
            | x == "-f" || x == "--file-type" =
                case xs of
                    []               -> Nothing
                    ("--" : _)       -> Nothing
                    (fileType : xs') -> go xs' $ partial { fileTypes' = fileType : fileTypes' partial }
            | x == "-c" || x == "--compiled" =
                go xs $ partial { shouldCompile' = True }
            | otherwise =
                case mainFileName' partial of
                    Nothing -> go xs $ partial { mainFileName' = Just x }
                    Just _  -> go xs $ partial { includeDirs' = x : includeDirs' partial}

fromPartial :: PartialArgs -> Maybe Args
fromPartial PartialArgs {..} =
    case mainFileName' of
        Nothing -> Nothing
        Just mfn -> Just Args
                            { mainFileName  = mfn
                            , includeDirs   = includeDirs'
                            , fileTypes     = fileTypes'
                            , targetArgs    = targetArgs'
                            , shouldCompile = shouldCompile'
                            }

