module Compiler.Builtin.Tokens
    ( BIErr
    , addAdts
    , addProps
) where

import Lib.Result
import qualified Compiler.State as With
import qualified Compiler.Ast.Tree as Raw
import qualified Compiler.Config.Types as BITy
import qualified Compiler.Config.Props as BIProp
import Text.Parsec
import Compiler.Syntax.Grammar
import Compiler.Syntax.Lib.Info

newtype BIErr =
      ParseErr ParseError

instance InfoShow BIErr where
    infoShow (ParseErr _) = unexpNoInfo

instance DebugShow BIErr where
    dbgShow (ParseErr err) = show err

instance UnreachableState BIErr where
    isUnreachable (ParseErr err) = Just $ show err

biAdtsSourceName :: String
biAdtsSourceName = "Builtin types source"

biPropsSourceName :: String
biPropsSourceName = "Builtin properties source"

parseAdts :: Either BIErr [Raw.AlgebraicDataType With.ProgState]
parseAdts =
    case runParser (many specialAdt') makeInitialStatus biAdtsSourceName $ getSource BITy.types of
        Left err -> Left $ ParseErr err
        Right props -> Right props
    where
        getSource :: [BITy.Type] -> String
        getSource [] = ""
        getSource (ty : t) = BITy.source ty ++ getSource t

addAdts :: Raw.Program With.ProgState -> Either BIErr (Raw.Program With.ProgState)
addAdts p =
    case parseAdts of
        Left err -> Left err
        Right adts -> Right $ Raw.mergePrograms (Raw.buildProgram $ map Raw.ADT adts) p

parseProps :: Either BIErr [Raw.Interface With.ProgState]
parseProps =
    case runParser (many interface) makeInitialStatus biPropsSourceName $ getSource BIProp.props of
        Left err -> Left $ ParseErr err
        Right props -> Right props
    where
        getSource :: [BIProp.Prop] -> String
        getSource [] = ""
        getSource (p : t) = BIProp.source p ++ getSource t

addProps :: Raw.Program With.ProgState -> Either BIErr (Raw.Program With.ProgState) 
addProps p =
    case parseProps of
        Left err -> Left err
        Right props -> Right $ Raw.mergePrograms (Raw.buildProgram $ map Raw.Intf props) p