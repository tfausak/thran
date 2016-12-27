module Thran where

import Prelude

import Data.Argonaut as Argonaut
import Data.Array as Array
import Data.Either as Either
import Data.Int as Int
import Data.Maybe as Maybe
import Data.String as String
import Data.StrMap as StrMap
import Data.Traversable as Traversable
import Data.Tuple as Tuple

-- Types

data Module = Module
  { name :: String
  , pscVersion :: String
  , exports :: Array String
  , imports :: Array String
  , foreignImports :: Array String
  , declarations :: Array Declaration
  }

data Declaration = Declaration
  { name :: String
  , expression :: Expression
  }

data Expression
  = ApplicationExpression
    { left :: Expression
    , right :: Expression
    }
  | CaseExpression
    { expressions :: Array Expression
    , alternatives :: Array Alternative
    }
  | FunctionExpression
    { name :: String
    , body :: Expression
    }
  | LiteralExpression
    { literal :: Literal
    }
  | VariableExpression
    { name :: String
    }

data Alternative = Alternative
  { binders :: Array Binder
  , body :: Expression
  }

data Binder
  = LiteralBinder
    { literal :: Literal
    }
  | VariableBinder
    { name :: String
    }

data Literal
  = ArrayLiteral
    { value :: Array Expression
    }
  | BooleanLiteral
    { value :: Boolean
    }
  | CharLiteral
    { value :: Char
    }
  | IntegerLiteral
    { value :: Int
    }
  | NumberLiteral
    { value :: Number
    }
  | StringLiteral
    { value :: String
    }

-- Compilation

compile :: Argonaut.Json -> Either.Either String String
compile json = do
  module_ <- Argonaut.decodeJson json
  compileModule module_

compileModule :: Module -> Either.Either String String
compileModule (Module module_) = do
  exports <- Traversable.traverse compileIdentifier module_.exports
  declarations <- Traversable.traverse compileDeclaration module_.declarations
  Either.Right (String.joinWith ""
    [ "{-# LANGUAGE NoImplicitPrelude #-}\n"
    , "-- Built with psc version ", module_.pscVersion, ".\n"
    , "module ", module_.name, "\n"
    , "(", String.joinWith ", " exports, ")\n"
    , "where\n"
    , "import qualified Prelude\n"
    , String.joinWith "" declarations
    ])

compileDeclaration :: Declaration -> Either.Either String String
compileDeclaration (Declaration declaration) = do
  name <- compileIdentifier declaration.name
  expression <- compileExpression declaration.expression
  Either.Right (String.joinWith "" [name, " = ", expression, "\n"])

compileExpression :: Expression -> Either.Either String String
compileExpression expression = case expression of
  ApplicationExpression { left, right } -> do
    compiledLeft <- compileExpression left
    compiledRight <- compileExpression right
    Either.Right (String.joinWith "" ["(", compiledLeft, " ", compiledRight, ")"])
  CaseExpression { expressions, alternatives } -> do
    compiledExpressions <- Traversable.traverse compileExpression expressions
    compiledAlternatives <- Traversable.traverse compileAlternative alternatives
    Either.Right (String.joinWith ""
      [ "(case ("
      , String.joinWith ", " compiledExpressions
      , ") of { "
      , String.joinWith "; " compiledAlternatives
      , " })"
      ])
  FunctionExpression { name, body } -> do
    compiledName <- compileIdentifier name
    compiledBody <- compileExpression body
    Either.Right (String.joinWith "" ["(\\ ", compiledName, " -> ", compiledBody, ")"])
  LiteralExpression { literal } -> compileLiteral literal
  VariableExpression { name } -> compileIdentifier name

compileAlternative :: Alternative -> Either.Either String String
compileAlternative (Alternative alternative) = do
  compiledBinders <- Traversable.traverse compileBinder alternative.binders
  compiledBody <- compileExpression alternative.body
  Either.Right (String.joinWith ""
    [ "("
    , String.joinWith ", " compiledBinders
    , ") -> "
    , compiledBody
    ])

compileBinder :: Binder -> Either.Either String String
compileBinder binder = case binder of
  LiteralBinder { literal } -> compileLiteral literal
  VariableBinder { name } -> compileIdentifier name

compileLiteral :: Literal -> Either.Either String String
compileLiteral literal = case literal of
  ArrayLiteral { value } -> do
    elements <- Traversable.traverse compileExpression value
    Either.Right (String.joinWith "" ["[", String.joinWith ", " elements, "]"])
  BooleanLiteral { value } -> case value of
    false -> Either.Right "Prelude.False"
    true -> Either.Right "Prelude.True"
  CharLiteral { value } -> Either.Right (show value)
  IntegerLiteral { value } -> Either.Right (show value)
  NumberLiteral { value } -> Either.Right (show value)
  StringLiteral { value } -> Either.Right (show value)

compileIdentifier :: String -> Either.Either String String
compileIdentifier identifier = Either.Right identifier

-- JSON

instance decodeJsonModule :: Argonaut.DecodeJson Module where
  decodeJson json = do
    object <- toEither "corefn not object" (Argonaut.toObject json)
    Tuple.Tuple name moduleJson <- toEither "corefn object not singleton" (fromSingleton object)
    moduleObject <- toEither "module not object" (Argonaut.toObject moduleJson)
    pscVersion <- Argonaut.getField moduleObject "builtWith"
    exports <- Argonaut.getField moduleObject "exports"
    imports <- Argonaut.getField moduleObject "imports"
    foreignImports <- Argonaut.getField moduleObject "foreign"
    declarations <- Argonaut.getField moduleObject "decls"
    Either.Right (Module { name, pscVersion, exports, imports, foreignImports, declarations })

instance decodeJsonDeclaration :: Argonaut.DecodeJson Declaration where
  decodeJson json = do
    object <- toEither "declaration not object" (Argonaut.toObject json)
    Tuple.Tuple name expressionJson <- toEither "declaration object not singleton" (fromSingleton object)
    expression <- Argonaut.decodeJson expressionJson
    Either.Right (Declaration { name, expression })

instance decodeJsonExpression :: Argonaut.DecodeJson Expression where
  decodeJson json = do
    array <- toEither "expression not array" (Argonaut.toArray json)
    { head, tail } <- toEither "expression array empty" (Array.uncons array)
    kind <- toEither "expression kind not string" (Argonaut.toString head)
    case kind of
      "Abs" -> decodeFunctionExpression tail
      "App" -> decodeApplicationExpression tail
      "Case" -> decodeCaseExpression tail
      "Literal" -> decodeLiteralExpression tail
      "Var" -> decodeVariableExpression tail
      _ -> Either.Left "unknown expression"

decodeApplicationExpression :: Array Argonaut.Json -> Either.Either String Expression
decodeApplicationExpression array = do
  case array of
    [first, second] -> do
      left <- Argonaut.decodeJson first
      right <- Argonaut.decodeJson second
      Either.Right (ApplicationExpression { left, right })
    _ -> Either.Left "invalid application"

decodeCaseExpression :: Array Argonaut.Json -> Either.Either String Expression
decodeCaseExpression array = do
  case array of
    [first, second] -> do
      expressions <- Argonaut.decodeJson first
      alternatives <- Argonaut.decodeJson second
      Either.Right (CaseExpression { expressions, alternatives })
    _ -> Either.Left "invalid case"

decodeFunctionExpression :: Array Argonaut.Json -> Either.Either String Expression
decodeFunctionExpression array = do
  { head, tail } <- toEither "function array empty" (Array.uncons array)
  name <- toEither "function name not string" (Argonaut.toString head)
  body <- case tail of
    [element] -> Argonaut.decodeJson element
    _ -> Either.Left "invalid function body"
  Either.Right (FunctionExpression { name, body })

decodeLiteralExpression :: Array Argonaut.Json -> Either.Either String Expression
decodeLiteralExpression array = do
  json <- case array of
    [element] -> Either.Right element
    _ -> Either.Left "invalid literal"
  literal <- Argonaut.decodeJson json
  Either.Right (LiteralExpression { literal })

decodeVariableExpression :: Array Argonaut.Json -> Either.Either String Expression
decodeVariableExpression array = do
  case array of
    [element] -> do
      name <- toEither "variable name not string" (Argonaut.toString element)
      Either.Right (VariableExpression { name })
    _ -> Either.Left "invalid variable"

instance decodeJsonAlternative :: Argonaut.DecodeJson Alternative where
  decodeJson json = do
    array <- toEither "alternative not array" (Argonaut.toArray json)
    case array of
      [first, second] -> do
        binders <- Argonaut.decodeJson first
        body <- Argonaut.decodeJson second
        Either.Right (Alternative { binders, body })
      _ -> Either.Left "invalid alternative"

instance decodeJsonBinder :: Argonaut.DecodeJson Binder where
  decodeJson json = do
    array <- toEither "binder not array" (Argonaut.toArray json)
    { head, tail } <- toEither "binder array empty" (Array.uncons array)
    kind <- toEither "binder kind not string" (Argonaut.toString head)
    case kind of
      "LiteralBinder" -> case tail of
        [element] -> do
          literal <- Argonaut.decodeJson element
          Either.Right (LiteralBinder { literal })
        _ -> Either.Left "invalid literal binder"
      "VarBinder" -> case tail of
        [element] -> do
          name <- toEither "variable binder name not string" (Argonaut.toString element)
          Either.Right (VariableBinder { name })
        _ -> Either.Left "invalid variable binder"
      _ -> Either.Left "unknown binder"

instance decodeJsonLiteral :: Argonaut.DecodeJson Literal where
  decodeJson json = do
    array <- toEither "literal not array" (Argonaut.toArray json)
    { head, tail } <- toEither "literal array empty" (Array.uncons array)
    kind <- toEither "literal kind not string" (Argonaut.toString head)
    case kind of
      "ArrayLiteral" -> decodeArrayLiteral tail
      "BooleanLiteral" -> decodeBooleanLiteral tail
      "CharLiteral" -> decodeCharLiteral tail
      "IntLiteral" -> decodeIntegerLiteral tail
      "NumberLiteral" -> decodeNumberLiteral tail
      "StringLiteral" -> decodeStringLiteral tail
      _ -> Either.Left "unknown literal"

decodeArrayLiteral :: Array Argonaut.Json -> Either.Either String Literal
decodeArrayLiteral array = do
  json <- case array of
    [element] -> Either.Right element
    _ -> Either.Left "invalid array value"
  arrayJson <- toEither "array json not array" (Argonaut.toArray json)
  value <- Traversable.traverse Argonaut.decodeJson arrayJson
  Either.Right (ArrayLiteral { value })

decodeBooleanLiteral :: Array Argonaut.Json -> Either.Either String Literal
decodeBooleanLiteral array = do
  json <- case array of
    [element] -> Either.Right element
    _ -> Either.Left "invalid boolean value"
  value <- toEither "boolean json not boolean" (Argonaut.toBoolean json)
  Either.Right (BooleanLiteral { value })

decodeCharLiteral :: Array Argonaut.Json -> Either.Either String Literal
decodeCharLiteral array = do
  json <- case array of
    [element] -> Either.Right element
    _ -> Either.Left "invalid character value"
  string <- toEither "character json not string" (Argonaut.toString json)
  value <- toEither "" (String.toChar string)
  Either.Right (CharLiteral { value })

decodeIntegerLiteral :: Array Argonaut.Json -> Either.Either String Literal
decodeIntegerLiteral array = do
  json <- case array of
    [element] -> Either.Right element
    _ -> Either.Left "invalid integer value"
  value <- toEither "integer json not number" (Argonaut.toNumber json)
  Either.Right (IntegerLiteral { value: Int.round value })

decodeNumberLiteral :: Array Argonaut.Json -> Either.Either String Literal
decodeNumberLiteral array = do
  json <- case array of
    [element] -> Either.Right element
    _ -> Either.Left "invalid number value"
  value <- toEither "number json not number" (Argonaut.toNumber json)
  Either.Right (NumberLiteral { value })

decodeStringLiteral :: Array Argonaut.Json -> Either.Either String Literal
decodeStringLiteral array = do
  json <- case array of
    [element] -> Either.Right element
    _ -> Either.Left "invalid string value"
  value <- toEither "string json not string" (Argonaut.toString json)
  Either.Right (StringLiteral { value })

-- Helpers

toEither :: forall a b. a -> Maybe.Maybe b -> Either.Either a b
toEither left maybeRight = case maybeRight of
  Maybe.Just right -> Either.Right right
  Maybe.Nothing -> Either.Left left

fromSingleton :: forall a. StrMap.StrMap a -> Maybe.Maybe (Tuple.Tuple String a)
fromSingleton strMap = case StrMap.toUnfoldable strMap of
  [element] -> Maybe.Just element
  _ -> Maybe.Nothing
