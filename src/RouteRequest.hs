module RouteRequest (routeRequest) where

import           Data.Aeson                 (Value (String), encode, object)
import           Data.Aeson.Key             (fromString)
import           Data.ByteString.Lazy.Char8 (unpack)
import           Data.List                  (intercalate)
import           Data.Text                  (pack)
import           Data.Time.Clock            (getCurrentTime)
import           Data.Time.Clock.POSIX      (utcTimeToPOSIXSeconds)
import           JSON                       (JsonValue (..))
import           Wordle                     (checkValid, ensureCriteria,
                                             generateRandomAnswer, makeFeedback)

-- | Generates a random target and returns it as a key-value pair in the form of
--   a list containing a tuple. The key is always "target", and the value is
--   the generated random answer.
--
-- This function first obtains a time seed using `getTimeSeed`, then calls
-- `generateRandomAnswer` using this seed. The result of `generateRandomAnswer`
-- is a tuple, and the second element (the random answer) is extracted and returned
-- in the form of a list containing a key-value pair.
--
-- Example usage:
--
-- >>> getTarget
-- [("target",...)]
--
-- (The actual value depends on the result of `generateRandomAnswer`.)
getTarget :: IO [(String, String)]
getTarget = do
  seed <- getTimeSeed                 -- Get a time-based seed
  (_, randomAnswer) <- generateRandomAnswer seed  -- Generate the random Wordle answer using the seed
  return [("target", randomAnswer)]    -- Return the result as a list with a key-value pair

-- | Processes a JSON input containing a guess, target, and optional previous guess.
--   Based on the criteria and validation, it returns feedback and the validity of the guess.
-- Helper function hints:
-- getOptionalString :: Maybe JSON -> Maybe String
-- getKey :: String -> JSON -> Maybe JSON
-- >>> getFeedback (JObject [("guess", JString "abcde"), ("target", JString "sheep"), ("previousGuess", JString "plate")])
-- [("feedback","[\"gray\",\"gray\",\"gray\",\"gray\",\"yellow\"]"),("valid","False")]
-- >>> getFeedback (JObject [("guess", JString "plate"), ("target", JString "sheep"), ("previousGuess", JNull)])
-- [("feedback","[\"yellow\",\"gray\",\"gray\",\"gray\",\"yellow\"]"),("valid","True")]
getFeedback :: JsonValue -> IO [(String, String)]
getFeedback json = do
  -- Extract guess, target, and optional previous guess from the JSON input
  let guess = getString (getKey "guess" json)
      target = getString (getKey "target" json)
      previousGuess = getOptionalString (getKey "previousGuess" json)

  -- Generate feedback based on the guess and target
  let feedback = makeFeedback guess target
      isValid = ensureCriteria previousGuess guess target

  -- Convert feedback and validity to string form
  let feedbackStr = "[" ++ intercalate "," (map (show . show) feedback) ++ "]"  -- Serialize feedback into the expected string format
      validStr = show isValid

  -- Return the results as a key-value pair list
  return [("feedback", feedbackStr), ("valid", validStr)]




-- | Routes HTTP requests based on the request type and site.
--
-- The function takes a tuple representing the HTTP request method (`req`),
-- the requested site (`site`), and a `JsonValue` (`args`) which contains
-- additional arguments (for POST requests). It returns a `Maybe String` wrapped
-- in an `IO` action, where `Nothing` is returned if the request is unrecognized.
--
-- This function supports the following routes:
--
-- 1. **GET /api/getTarget**: Retrieves the target data via the `getTarget` function
--    and returns it as an HTTP response.
-- 2. **POST /api/makeGuess**: Processes the arguments via the `getFeedback` function
--    and returns the result as an HTTP response.
--
-- The response is generated using the `createHttpResponseFromPairs` function to convert
-- the result into a valid HTTP response.
--
-- Example usage:
--
-- >>> routeRequest ("GET", "/api/getTarget", JNull)
-- Just "HTTP/1.1 200 OK\r\nContent-Type: ...\r\n\r\n{\"target\":\"...\"}\r\n"
--
-- >>> routeRequest ("POST", "/api/makeGuess", JObject [("guess", JString "abcde"), ("target", JString "sheep"), ("previousGuess", JString "plate")])
-- Just "HTTP/1.1 200 OK\r\nContent-Type: application/json\r\nContent-Length: 79\r\n\r\n{\"feedback\":\"[\\\"gray\\\",\\\"gray\\\",\\\"gray\\\",\\\"gray\\\",\\\"yellow\\\"]\",\"valid\":\"False\"}\r\n"
--
-- >>> routeRequest ("PUT", "/api/unknown", JObject [])
-- Nothing
routeRequest :: (String, String, JsonValue) -> IO (Maybe String)
routeRequest (req, site, args) = case (req, site) of
    -- GET request for retrieving the target word
    ("GET", "/api/getTarget") -> do
      Just . createHttpResponseFromPairs <$> getTarget

    -- POST request for processing a guess and generating feedback
    ("POST", "/api/makeGuess") -> do
      feedbackData <- getFeedback args
      return $ Just (createHttpResponseFromPairs feedbackData)

    -- Unrecognized route
    _ -> return Nothing


-- | Extract a string from a JSON value, throwing an error if the value is not a string.
--
-- >>> getString (Just (JString "hello"))
-- "hello"
getString :: Maybe JsonValue -> String
getString (Just (JString s)) = s
getString _                  = error "Expected a JString, but got something else"

-- | Get the value corresponding to a key in a JSON object.
--
-- >>> let json = JObject [("key1", JString "value1"), ("key2", JString "value2")]
-- >>> getKey "key1" json
-- Just (JString "value1")
-- >>> getKey "key3" json
-- Nothing
getKey :: String -> JsonValue -> Maybe JsonValue
getKey k (JObject kv) = lookup k kv
getKey _ _            = Nothing

-- | Wraps a function that extracts a value from a JSON value in a `Maybe`,
--   returning `Nothing` if the JSON value is `null`.
getOptional :: (Maybe JsonValue -> a) -> Maybe JsonValue -> Maybe a
getOptional _ (Just JNull) = Nothing
getOptional f x            = Just $ f x

-- | Extract a string or null from a JSON value, throwing an error if the value
--   is neither a string or null.
--
-- >>> getOptionalString (Just (JString "hello"))
-- Just "hello"
-- >>> getOptionalString (Just JNull)
-- Nothing
getOptionalString :: Maybe JsonValue -> Maybe String
getOptionalString = getOptional getString

-- Convert the current time to an integer (number of picoseconds since the epoch)
getTimeSeed :: IO Int
getTimeSeed = fromEnum . utcTimeToPOSIXSeconds <$> getCurrentTime


-- | Encode a list of key-value string pairs into a JSON string.
--
-- >>> encodePairsToJsonString [("name", "John"), ("age", "30")]
-- "{\"age\":\"30\",\"name\":\"John\"}"
encodePairsToJsonString :: [(String, String)] -> String
encodePairsToJsonString kvPairs =
    unpack $ encode $ object $ map (\(k, v) -> (fromString k, String (pack v))) kvPairs


createHttpResponseFromPairs :: [(String, String)] -> String
createHttpResponseFromPairs kvPairs =
    intercalate "\r\n"
      ["HTTP/1.1 200 OK",
      "Content-Type: application/json",
      "Content-Length: " ++ contentLength,
      "",
      encodedString,
      ""
      ]
  where
    encodedString = encodePairsToJsonString kvPairs
    contentLength = show (length encodedString)
