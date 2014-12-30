module Encryption
  ( encryptAll
  , decryptAll
  , getEncDir
  , getDecDir
  ) where

import Control.Applicative
import Control.Monad
import Crypto.Hash.MD5
import Data.List
import Data.Maybe
import Data.Monoid
import Crypto.Random.DRBG
import Crypto.Cipher.AES
import System.Directory
import System.Exit
import System.FilePath
import qualified Data.ByteString.Char8 as BS

encryptAll :: FilePath -> IO ()
encryptAll passFile = do
  sourceDir <- getDecDir
  targetDir <- getEncDir
  pass      <- normalizePassword passFile
  sourceFiles  <- listAllFiles sourceDir
  forM_ sourceFiles $ \ source -> do
    let relPath = fromJust $ stripPrefix (sourceDir ++ "/") source
        target  = targetDir </> relPath <.> "enc"
        doIt    = createParents target >> encryptToFile pass source target
    checkFile <- doesFileExist target
    if checkFile
      then do
        sourceHash <- hash <$> BS.readFile source
        targetHash <- do
          clear <- fromMaybe "" <$> decryptContent pass target
          return $ hash clear
        if (sourceHash == targetHash)
          then putStrLn $ target ++ " looks up to date; skipping."
          else (putStrLn $ source ++ " changed. Reencrypting") >> doIt
      else do
        putStrLn $ "Encrypting " ++ source
        doIt

decryptAll :: FilePath -> IO ()
decryptAll passFile = do
  targetDir   <- getDecDir
  sourceDir   <- getEncDir
  pass        <- normalizePassword passFile
  sourceFiles <- listAllFiles sourceDir
  forM_ sourceFiles $ \ source -> do
    let relPath = fromJust $ stripPrefix (sourceDir ++ "/") source
        target  = dropExtension $ targetDir </> relPath
        failure = putStrLn ("Error while decrypting " ++ source ++ "\nAborting") >> exitFailure
        success c = createParents target >> putStrLn ("Decrypted " ++ source ++ " successfully.") >> BS.writeFile target c
    checkFile <- doesFileExist target
    if checkFile
      then do
        mClear <- decryptContent pass source
        case mClear of
          Nothing -> failure
          Just c  -> do
            let hashSource = hash c
            hashTarget <- hash <$> BS.readFile target
            if hashSource == hashTarget
              then putStrLn ("The file " ++ target ++ " did not change; skipping")
              else putStrLn (source ++ " changed.") >> success c
      else do
        putStrLn $ "Decrypting " ++ source
        mClear <- decryptContent pass source
        case mClear of
          Nothing -> failure
          Just c  -> success c

createParents :: FilePath -> IO ()
createParents fp = createDirectoryIfMissing True $  takeDirectory fp

getEncDir :: IO FilePath
getEncDir = (</> "secrets") <$> getCurrentDirectory

getDecDir :: IO FilePath
getDecDir = (</> "clear_secrets") <$> getCurrentDirectory

listAllFiles :: FilePath -> IO [FilePath]
listAllFiles seed = do
  let go acc [] = return acc
      go acc (dir : dirs) = do
        newItems <- (filter (not . ("." `isPrefixOf`))) <$> getDirectoryContents dir
        newDirs  <- filterM doesDirectoryExist $ map (dir </>) newItems
        newFiles <- filterM doesFileExist $ map (dir </>) newItems
        go (acc ++ newFiles) (dirs ++ newDirs)
  go [] [seed]


-- ^ get a password from a path and transform it
-- into a 16 bytes bytestring (if necessary
-- by repeating and truncating the input)
normalizePassword :: FilePath -> IO BS.ByteString
normalizePassword passFile = do
    pass <- BS.readFile passFile
    let complete s = if BS.length s >= 32 then BS.take 32 s else complete (s <> s)
    return $ complete pass

encryptToFile :: BS.ByteString -- ^ The password
              -> FilePath      -- ^ File to encrypt
              -> FilePath      -- ^ Target
              -> IO ()
encryptToFile pass source target = do
  decContent <- BS.readFile source
  let encContent = encrypt pass decContent
  BS.writeFile target =<< encContent

decryptContent :: BS.ByteString            -- ^ The password
               -> FilePath                 -- ^ File to decrypt
               -> IO (Maybe BS.ByteString) -- ^ The clear content of the file
decryptContent pass source = decrypt pass <$> BS.readFile source

createTargetDirectory :: FilePath -> IO ()
createTargetDirectory target = createDirectoryIfMissing True (takeDirectory target)

encrypt :: BS.ByteString    -- ^ The password (32 bytes)
        -> BS.ByteString    -- ^ The clear bytestring
        -> IO BS.ByteString -- ^ The encrypted bytestring
encrypt pass decContent = do
  let cipher = initAES pass
  ivStr <- randomByteString 16 -- The initialization vector for the CBC encryption
  let iv        = aesIV_ ivStr
      encrypted = encryptCBC cipher iv $ pad decContent
  return $ ivStr <> wrapStart <> encrypted

decrypt :: BS.ByteString       -- ^ The password (32 bytes)
        -> BS.ByteString       -- ^ The encrypted bytestring, with the form
                               -- thisistheivBEGIN_CONTENTencryptedcontent
        -> Maybe BS.ByteString -- ^ The decrypted bytestring
decrypt pass wrapped =
  let (ivStr, prefixed) = BS.breakSubstring wrapStart wrapped
      enc               = BS.drop (BS.length wrapStart) prefixed
      cipher            = initAES pass
      iv                = aesIV_ ivStr
      padded            = decryptCBC cipher iv enc
  in  unpad padded

wrapStart :: BS.ByteString
wrapStart = "BEGIN_CONTENT"

wrapEnd :: BS.ByteString
wrapEnd = "END_CONTENT"

-- | wrap the content like so: bla -> blaEND_CONTENT00000
-- So that the whole bytestring has a size multiple of 16
pad :: BS.ByteString -> BS.ByteString
pad str =
  let suffixed = str <> wrapEnd
      l        = BS.length suffixed
  in suffixed <> BS.replicate (16 - l `rem` 16) '0'

unpad :: BS.ByteString -> Maybe BS.ByteString
unpad str =
  let unpadded = BS.reverse . BS.dropWhile (== '0') . BS.reverse $ str
  in  if wrapEnd `BS.isSuffixOf` unpadded
        then Just $ BS.take (BS.length unpadded - BS.length wrapEnd) unpadded
        else Nothing

-- | Random hexa ByteString of size n generator
randomByteString :: Int -> IO BS.ByteString
randomByteString n = do
  gen <- newGenIO :: IO CtrDRBG
  case genBytes n gen of
    Right (bs, _) -> return $ BS.take n bs
    Left _        -> error "Error while trying to generate a random string!"
