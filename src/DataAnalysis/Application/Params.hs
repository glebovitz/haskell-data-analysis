{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

-- | Parameter parsing/template haskell code.

module DataAnalysis.Application.Params where

import           Control.Applicative

import           "mtl" Control.Monad.Error ()
import           Data.Char
import           Data.Maybe
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T

-- | A parameter specification.
data Parameter = Parameter
  { paramName :: !String
  , paramType :: !String
  , paramMin  :: !(Maybe String)
  , paramMax  :: !(Maybe String)
  , paramDesc :: !String
  , paramDef  :: !(Maybe String)
  }

-- | Parse a parameter spec from tokens.
fromTokens :: [Token] -> Either String Parameter
fromTokens xs@(Token name:Spaces{}:Token typ:_) =
  Parameter
    <$> pure (T.unpack name)
    <*> pure (T.unpack typ)
    <*> opt asis "min"
    <*> opt asis "max"
    <*> text "desc"
    <*> opt asis "default"
  where asis = Right
        opt :: (String -> Either String a) -> Text -> Either String (Maybe a)
        opt cont name = do mv <- optional (text name)
                           case mv of
                             Nothing -> return Nothing
                             Just t -> fmap Just (cont t)
        text :: Text -> Either String String
        text name =
          maybe (Left ("Expected argument: " <> T.unpack name <> "=<value>"))
                (Right . T.unpack)
                (listToMaybe (mapMaybe match xs))
          where match tok =
                  case tok of
                    Token t ->
                      case T.span (/='=') t of
                        (key,value)
                          | key == name && T.isPrefixOf "=" value -> Just (T.drop 1 value)
                        _ -> Nothing
                    _ -> Nothing
fromTokens _ = Left "Parameter format: <name> <type> [arg1=value1 argn=valuen …]"

-- | A token used by the parser.
data Token = Spaces !Int -- ^ @Spaces n@ are @n@ consecutive spaces.
           | Token Text  -- ^ @Token tok@ is token @tok@ already unquoted.
  deriving (Show, Eq)

-- | Tokenize a string.
tokenize :: Text -> [Token]
tokenize t
    | T.null t = []
    | "--" `T.isPrefixOf` t = [] -- Comment until the end of the line.
    | "#" `T.isPrefixOf` t = [] -- Also comment to the end of the
                                -- line, needed for a CPP bug (#110)
    | T.head t == '"' = quotes (T.tail t) id
    | T.head t == '(' = parens 1 (T.tail t) id
    | isSpace (T.head t) =
        let (spaces, rest) = T.span isSpace t
         in Spaces (T.length spaces) : tokenize rest

    -- support mid-token quotes and parens
    | Just (beforeEquals, afterEquals) <- findMidToken t
    , not (T.any isSpace beforeEquals)
    , Token next : rest <- tokenize afterEquals =
        Token (T.concat [beforeEquals, "=", next]) : rest

    | otherwise =
        let (token, rest) = T.break isSpace t
         in Token token : tokenize rest
  where
    findMidToken t' =
        case T.break (== '=') t' of
            (x, T.drop 1 -> y)
                | "\"" `T.isPrefixOf` y || "(" `T.isPrefixOf` y -> Just (x, y)
            _ -> Nothing

    quotes t' front
        | T.null t' = error $ T.unpack $ T.concat $
            "Unterminated quoted string starting with " : front []
        | T.head t' == '"' = Token (T.concat $ front []) : tokenize (T.tail t')
        | T.head t' == '\\' && T.length t' > 1 =
            quotes (T.drop 2 t') (front . (T.take 1 (T.drop 1 t'):))
        | otherwise =
            let (x, y) = T.break (`elem` "\\\"") t'
             in quotes y (front . (x:))
    parens count t' front
        | T.null t' = error $ T.unpack $ T.concat $
            "Unterminated parens string starting with " : front []
        | T.head t' == ')' =
            if count == (1 :: Int)
                then Token (T.concat $ front []) : tokenize (T.tail t')
                else parens (count - 1) (T.tail t') (front . (")":))
        | T.head t' == '(' =
            parens (count + 1) (T.tail t') (front . ("(":))
        | T.head t' == '\\' && T.length t' > 1 =
            parens count (T.drop 2 t') (front . (T.take 1 (T.drop 1 t'):))
        | otherwise =
            let (x, y) = T.break (`elem` "\\()") t'
             in parens count y (front . (x:))
