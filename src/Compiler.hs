{-# LANGUAGE TupleSections #-}

module Compiler
    {- Exposing each single functionality of the compiler -}
    ( compilerExecName
    , readSource
    , parsing
    , addBuiltinTypes
    , addBuiltinProps
    , namesCheck
    , argsCheck
    , aliasReplace
    , sigsReplace
    , rebindTyVars
    , constraintsCheck
    , inferKinds
    , buildCons
    , inferConstraints
    , instances
    , bindings
    , prepareTyInf
    , typeInference
    , adHocPolymorphism
    , makeLambdas
    , removeDeepPatternMatching
    , unifyScrutinee
    , genCore
    , backend
    , compile
) where

import Control.Monad(when)
import Lib.Monad.Utils
import Data.Maybe(isJust)
import System.Exit
import Lib.Utils
import Lib.Result
--import Text.Pretty.Simple(pPrint)
import qualified Lib.Counter as C
import qualified Compiler.State as With
import Compiler.Config.Lexer
import qualified Compiler.Builtin.Tokens as Builtin.Tokens
import qualified Compiler.Ast.Tree as Raw
import qualified Compiler.Syntax.Parser as Parser
import qualified Compiler.Names.Check as Names.Check
import qualified Compiler.Args.Check as Args.Check
import qualified Compiler.Desugar.Alias.Replace as Alias.Replace
import qualified Compiler.Desugar.Sigs as Sigs.Replace
import qualified Compiler.Desugar.TyVars as Rebind
import qualified Compiler.Desugar.Lambda as Lambdas
import qualified Compiler.Desugar.DeepPM as DeepPM
import qualified Compiler.Desugar.Scrutinee as Scrutinee
import qualified Compiler.Constraints.Check as ContCheck
import qualified Compiler.Types.Make as MkTy
import qualified Compiler.Desugar.AdHoc as AdHoc
import CoreSyn(CoreProgram)
import TyCon(TyCon)
import qualified Compiler.Codegen.ToCore as ToCore
import qualified ModuleSys.HsModl as HsModl
import GHC
import HscTypes
import IfaceSyn
import Var
import Avail
import FromCore
import OccName hiding (varName)
import FastString
import Fingerprint

compilerExecName :: String
compilerExecName = "mylang-exe"

evalPhaseWith
    :: UnreachableState err
    => a
    -> (a -> Either err b)
    -> (b -> IO ())
    -> (b -> c)
    -> (err -> IO ())
    -> IO c    --Error case
    -> IO c
evalPhaseWith input eval doOn f doOnErr doErr =
    case eval input of
        Right res ->
            do { doOn res
               ; return $ f res
               }
        Left err ->
            do { doOnErr err
               ; when <| isJust (isUnreachable err) <| print "PANIC"
               ; doErr
               }

evalSuccessPhaseWith
    :: a
    -> (a -> b)
    -> (b -> IO ())
    -> (b -> c)
    -> IO c
evalSuccessPhaseWith input eval doOn f = do
    let res = eval input
    doOn res
    return $ f res

evalPhase
    :: UnreachableState err
    => a
    -> (a -> Either err b)
    -> (b -> IO ())
    -> (err -> IO ())
    -> IO b    --Error case
    -> IO b
evalPhase input eval doOn = evalPhaseWith input eval doOn id

evalDbgPhase
    :: (DebugShow err, UnreachableState err)
    => a
    -> (a -> Either err b)
    -> (b -> IO ())
    -> IO b    --Error case
    -> IO b
evalDbgPhase input eval doOn = evalDbgPhaseWith input eval doOn id

evalDbgPhaseWith
    :: (DebugShow err, UnreachableState err)
    => a
    -> (a -> Either err b)
    -> (b -> IO ())
    -> (b -> c)
    -> IO c    --Error case
    -> IO c
evalDbgPhaseWith input eval doOn f = evalPhaseWith input eval doOn f (putStrLn . dbgShow)

prNone :: a -> IO ()
prNone = const doNothing'

readSource :: FilePath -> IO String
readSource = readFile

parsing :: FilePath -> IO (Raw.Program With.ProgState)
parsing path = do
    input <- readSource path
    evalDbgPhase input (Parser.program path) prNone exitFailure

addBuiltinTypes :: FilePath -> IO (Raw.Program With.ProgState)
addBuiltinTypes path = do
    p <- parsing path
    evalDbgPhase p Builtin.Tokens.addAdts prNone exitFailure

addBuiltinProps :: FilePath -> IO (Raw.Program With.ProgState)
addBuiltinProps path = do
    p <- addBuiltinTypes path
    evalDbgPhase p Builtin.Tokens.addProps prNone exitFailure

namesCheck :: FilePath -> IO (Raw.Program With.ProgState)
namesCheck path = do
    p <- addBuiltinProps path
    evalDbgPhase p Names.Check.perform prNone exitFailure
    return p

{- TODO: use Lib.Result API. -}
argsCheck :: FilePath -> IO (Raw.Program With.ProgState)
argsCheck path = do
    p <- namesCheck path
    case Args.Check.perform p of
        Right _ -> do { print "Arguments check ok"
                      ; return p
                      }
        --TODO: this piece of code is a bug, use Lib.Result utilities in Args module
        Left (Args.Check.AdtErr err) -> do { print $ Args.Check.AdtErr err
                                           ; exitFailure
                                           }
        Left (Args.Check.AliasErr err) -> do { print $ Args.Check.AliasErr err
                                             ; exitFailure
                                             }
        Left other -> do { print other
                         ; print "PANIC"
                         ; exitFailure
                         }

{- TODO: use Lib.Result API. -}
aliasReplace :: FilePath -> IO (Raw.Program With.ProgState)
aliasReplace path = do
    p <- argsCheck path
    case Alias.Replace.perform p of
        This p' -> do { print "Aliases replacing"
                      ; return p'
                      }
        That cycle -> do { Alias.Replace.errPrint $ That cycle
                         ; exitFailure
                         }
        None -> do { Alias.Replace.errPrint None
                   ; print "PANIC"
                   ; exitFailure
                   }

{- TODO: use Lib.Result API. -}
sigsReplace :: FilePath -> IO (Raw.Program With.ProgState)
sigsReplace path = do
    p <- aliasReplace path
    print "Signatures replacing"
    case Sigs.Replace.perform p of
        Right p' -> return p'
        Left _ -> do { print "Unreachable state: PANIC"
                     ; exitFailure
                     }

rebindTyVars :: FilePath -> IO (MkTy.FV (), Raw.Program With.ProgState)
rebindTyVars path = do
    p <- sigsReplace path
    print "Rebinding type variables"
    evalDbgPhase p Rebind.updateTyVars prNone exitFailure

constraintsCheck :: FilePath -> IO (MkTy.FV (), Raw.Program With.ProgState)
constraintsCheck path = do
    (fv, p) <- rebindTyVars path
    print "Constraints check"
    evalDbgPhaseWith p ContCheck.perform
        (const $ print "Constraints check terminated successfully")
        (fv, )
        exitFailure

inferKinds :: FilePath -> IO (MkTy.FV (), MkTy.TypesTable With.ProgState, Raw.Program With.ProgState)
inferKinds path = do
    (fv, p) <- constraintsCheck path
    print "Kind inference"
    evalDbgPhaseWith p MkTy.inferKind prNone (\(tt, p') -> (fv, tt, p')) exitFailure

buildCons
    :: FilePath
    -> IO
        ( MkTy.FV ()
        , MkTy.TypesTable With.ProgState
        , MkTy.DataConsTable With.ProgState
        , Raw.Program With.ProgState
        )
buildCons path = do
    (fv, tt, p) <- inferKinds path
    print "Building constructors"
    evalDbgPhaseWith tt (MkTy.buildCons p fv)
        prNone
        (\(dct, fv', p') -> (fv', tt, dct, p'))
        exitFailure

inferConstraints
    :: FilePath
    -> IO
        ( MkTy.FV ()
        , MkTy.TypesTable With.ProgState
        , MkTy.DataConsTable With.ProgState
        , MkTy.ConstraintsTable With.ProgState
        , Raw.Program With.ProgState
        )
inferConstraints path = do
    (fv, tt, dct, p) <- buildCons path
    print "Constraint inference"
    evalDbgPhaseWith tt (MkTy.inferConstraint p)
        prNone
        (\(ct, p') -> (fv, tt, dct, ct, p'))
        exitFailure

instances
    :: FilePath
    -> IO
        ( MkTy.FV ()
        , MkTy.TypesTable With.ProgState
        , MkTy.DataConsTable With.ProgState
        , MkTy.ConstraintsTable With.ProgState
        , MkTy.InstsTable With.ProgState
        , MkTy.PropMethodsTable With.ProgState
        , MkTy.ImplTable With.ProgState
        , Raw.Program With.ProgState
        )
instances path = do
    (fv, tt, dct, ct, p) <- inferConstraints path
    print "Instances variables creation"
    evalDbgPhaseWith fv (MkTy.mkInsts p tt ct)
        prNone
        (\(insts, mhts, it, fv', p') -> (fv', tt, dct, ct, insts, mhts, it, p'))
        exitFailure
    {-where
        printInstsRes (insts, it, _, _) = do
            print "Instances symbols"
            pPrint insts
            print "Implementations"
            pPrint it
    -}

bindings
    :: FilePath
    -> IO
        ( MkTy.FV ()
        , MkTy.TypesTable With.ProgState
        , MkTy.DataConsTable With.ProgState
        , MkTy.ConstraintsTable With.ProgState
        , MkTy.PropMethodsTable With.ProgState
        , MkTy.ImplTable With.ProgState
        , [Raw.SDUnion With.ProgState]
        )
bindings path = do
    (fv, tt, dct, ct, insts, mhts, it, p) <- instances path
    print "Fetching bindings from program and instances"
    evalSuccessPhaseWith p (MkTy.addInstsBindings insts . MkTy.getBindings)
        prNone
        (fv, tt, dct, ct, mhts, it, )

prepareTyInf
    :: FilePath
    -> IO
        ( MkTy.FV ()
        , MkTy.TypesTable With.ProgState
        , MkTy.DataConsTable With.ProgState
        , MkTy.ConstraintsTable With.ProgState
        , MkTy.PropMethodsTable With.ProgState
        , MkTy.ImplTable With.ProgState
        , MkTy.SortedBindings
        )
prepareTyInf path = do
    (fv, tt, dct, ct, mhts, it, bs) <- bindings path
    print "Preparing bindings for type inference"
    evalSuccessPhaseWith bs (MkTy.sortDefs mhts . MkTy.splitRec mhts . MkTy.disambiguateNested)
        prNone
        (fv, tt, dct, ct, mhts, it, )

typeInference
    :: FilePath
    -> IO
        ( MkTy.FV ()
        , MkTy.TypesTable With.ProgState
        , MkTy.DataConsTable With.ProgState
        , MkTy.ConstraintsTable With.ProgState
        , MkTy.PropMethodsTable With.ProgState
        , MkTy.ImplTable With.ProgState
        , MkTy.TypedProgram With.ProgState
        , C.AlphabeticCounterObj
        , [MkTy.BindingToRefine]
        )
typeInference path = do
    (fv, tt, dct, ct, mhts, it, bs) <- prepareTyInf path
    print "Type inference"
    evalDbgPhaseWith bs (MkTy.mkTypedProg fv tt dct ct mhts it)
        prNone
        (\(tp, fv', c, bstr) -> (fv', tt, dct, ct, mhts, it, tp, c, bstr))
        exitFailure

adHocPolymorphism
    :: FilePath
    -> IO
        ( MkTy.FV ()
        , MkTy.TypesTable With.ProgState
        , MkTy.DataConsTable With.ProgState
        , MkTy.ConstraintsTable With.ProgState
        , MkTy.PropMethodsTable With.ProgState
        , MkTy.ImplTable With.ProgState
        , MkTy.TypedProgram With.ProgState
        , C.AlphabeticCounterObj
        )
adHocPolymorphism path = do
    (fv, tt, dct, ct, mhts, it, tp, c, bstr) <- typeInference path
    evalDbgPhaseWith bstr (AdHoc.staticDispatch tp mhts)
        prNone
        (fv, tt, dct, ct, mhts, it,, c)
        exitFailure

makeLambdas
    :: FilePath
    -> IO
        ( MkTy.FV ()
        , MkTy.TypesTable With.ProgState
        , MkTy.DataConsTable With.ProgState
        , MkTy.ConstraintsTable With.ProgState
        , MkTy.PropMethodsTable With.ProgState
        , MkTy.ImplTable With.ProgState
        , MkTy.TypedProgram With.ProgState
        , C.AlphabeticCounterObj
        )
makeLambdas path = do
    (fv, tt, dct, ct, mhts, it, tp, c) <- adHocPolymorphism path
    print "Making lambda expressions"
    evalSuccessPhaseWith tp Lambdas.make
        prNone
        (fv, tt, dct, ct, mhts, it,, c)

removeDeepPatternMatching
    :: FilePath
    -> IO
        ( MkTy.FV ()
        , MkTy.TypesTable With.ProgState
        , MkTy.DataConsTable With.ProgState
        , MkTy.ConstraintsTable With.ProgState
        , MkTy.PropMethodsTable With.ProgState
        , MkTy.ImplTable With.ProgState
        , MkTy.TypedProgram With.ProgState
        , C.AlphabeticCounterObj
        )
removeDeepPatternMatching path = do
    (fv, tt, dct, ct, mhts, it, tp, c) <- makeLambdas path
    print "Removing deep pattern matching"
    evalDbgPhaseWith c (DeepPM.removeDeepPm tp)
        prNone
        (uncurry (fv, tt, dct, ct, mhts, it,, ))
        exitFailure

unifyScrutinee
    :: FilePath
    -> IO
        ( MkTy.FV ()
        , MkTy.TypesTable With.ProgState
        , MkTy.DataConsTable With.ProgState
        , MkTy.ConstraintsTable With.ProgState
        , MkTy.PropMethodsTable With.ProgState
        , MkTy.ImplTable With.ProgState
        , MkTy.TypedProgram With.ProgState
        , C.AlphabeticCounterObj
        )
unifyScrutinee path = do
    (fv, tt, dct, ct, mhts, it, tp, c) <- removeDeepPatternMatching path
    print "Unifying scrutinee in pattern matching constructs"
    evalSuccessPhaseWith c (Scrutinee.unify tp)
        prNone
        (uncurry (fv, tt, dct, ct, mhts, it,, ))

type MainBinding = Name

genCore :: FilePath -> IO (CoreProgram, [TyCon], ToCore.MainBndr)
genCore path = do
    (_, _, dct, _, _, _, tp, c) <- unifyScrutinee path
    print "Generating Core code"
    evalDbgPhaseWith tp (ToCore.generate dct c)
        prNone
        id
        exitFailure

mainHash :: IO Fingerprint
mainHash = return $ fingerprintString mainSymbol

modIfaceHash :: Warnings -> [(OccName, Fixity)] -> [(Fingerprint, IfaceDecl)] -> IO ModIfaceBackend
modIfaceHash ws fxs intfds =
    return $ ModIfaceBackend
        { mi_iface_hash = fingerprint0
        , mi_mod_hash = fingerprint0
        , mi_flag_hash = fingerprint0
        , mi_opt_hash = fingerprint0
        , mi_hpc_hash = fingerprint0
        , mi_plugin_hash = fingerprint0
        , mi_orphan = False
        , mi_finsts = False
        , mi_exp_hash = fingerprint0 --TODO
        , mi_orphan_hash = fingerprint0
        , mi_warn_fn = mkIfaceWarnCache ws
        , mi_fix_fn = mkIfaceFixCache fxs
        , mi_hash_fn = mkIfaceHashCache intfds
        }

modIface :: ToCore.MainBndr -> IO ModIface
modIface mainBndr = do
    let ws = NoWarnings
    let fxs = []
    mnHash <- mainHash
    let intfDecl =
         IfaceId
            { ifName = varName mainBndr
            , ifType = IfaceLitTy . IfaceStrTyLit $ fsLit mainSymbol
            , ifIdDetails = IfVanillaId
            , ifIdInfo = NoInfo
            }
    {- TODO: using such a `main` fingerprint? Is this the correct way to calculate this hash? -}
    let intfds = [(mnHash, intfDecl)]
    hash <- modIfaceHash ws fxs intfds
    return $ ModIface
        { mi_module = HsModl.mainModl
        , mi_sig_of = Nothing
        , mi_hsc_src = HsSrcFile
        , mi_deps = noDependencies
        , mi_usages = []
        , mi_exports = [Avail $ varName mainBndr]
        , mi_used_th = False
        , mi_fixities = fxs
        , mi_warns = ws
        , mi_anns = []
        , mi_decls = intfds
        , mi_globals = Nothing
        , mi_insts = []
        , mi_fam_insts = []
        , mi_rules = []
        , mi_hpc = False
        , mi_trust = noIfaceTrustInfo
        , mi_trust_pkg = False
        {- TODO: what is it? Cannot find anything from the documentation -}
        , mi_complete_sigs = []
        , mi_doc_hdr = Nothing
        , mi_decl_docs = emptyDeclDocMap
        , mi_arg_docs = emptyArgDocMap
        , mi_final_exts = hash
        }

backend :: FilePath -> HscTarget -> GhcLink -> IO ()
backend path target link = do
    (cp, tyCons, mnBndr) <- genCore path
    intf <- modIface mnBndr
    print "Compiler backend"
    coreCompUnit HsModl.mainModl target link (Just intf) cp tyCons []    --TODO: it misses instances

compile :: FilePath -> HscTarget -> GhcLink -> IO ()
compile = backend
