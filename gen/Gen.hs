{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative ((<$>), (<*>))
import Data.Aeson (eitherDecode)
import Data.Aeson.Types
import qualified Data.ByteString.Lazy as B
import Data.List (intersperse)
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T (pack)
import qualified Data.Text.Lazy as T (toStrict)
import qualified Data.Text.IO as T (writeFile)
import Data.Text.Template
import System.Environment (getArgs)

{- API model -}

newtype Structs = Structs { structs :: [StructGen]}
             deriving (Show)

data StructGen = SG {
  sgName :: StructName,
  sgFields :: [FieldGen]
  } deriving (Show)

data FieldGen = FG {
  fgName :: Text,
  fgType :: GenType,
  fgIgnore :: Bool
  } deriving (Show)

data Primitive = I | N | S | B

instance Show Primitive where
  show I = "integer"
  show N = "number"
  show S = "string"
  show B = "boolean"

data GenType = Prim Primitive
             | Struct StructName
             | PrimArray Primitive Int
             | StructArray Text Int
             | StructPointer Text
             deriving (Show)

{- Code generation -}

data Header
data Cpp

data Content a = Content Text deriving (Show)

data Output a = Output FilePath

newtype StructName = StructName { unStructName :: Text }
                   deriving (Show)

data CodeGen = CodeGen {
  forH :: Content Header,
  forCpp :: Content Cpp,
  forDispatchers :: [Dispatcher]
  } deriving (Show)

data Dispatcher = Dispatcher {
  dFunctionSig :: Text,
  dTableName :: Text,
  dFunctionName :: Text
  } deriving (Show)

instance Monoid CodeGen where
  mempty = CodeGen (Content "") (Content "") []
  (CodeGen h c d) `mappend` (CodeGen h' c' d') = CodeGen (h <.>  h')
                                                         (c <.> c')
                                                         (d <> d')

instance Monoid (Content a) where
  mempty = Content ""
  (Content t1) `mappend` (Content t2) = Content (t1 <> "\n\n" <> t2)

(<.>) :: Content a -> Content a -> Content a
(<.>) = (<>)

load :: IO CodeGen
load = do
  Right ss <- B.readFile "../model.json" >>= return . eitherDecode
  return (codegen ss)

main :: IO ()
main = do
  [model, header, cpp] <- getArgs
  let headerFile = Output header :: Output Header
      cppFile = Output cpp :: Output Cpp

  Right ss <- B.readFile model >>= return . eitherDecode
  let gen = codegen ss
      cppText = cppTop <.> forCpp gen
  writeContent cppText cppFile

  let headerText = headerTop <.> (genDispatchers . forDispatchers) gen <.> forH gen
  writeContent headerText headerFile

writeContent :: Content a -> Output a -> IO ()
writeContent (Content text) (Output path) = T.writeFile path text

headerTop :: Content Header
headerTop = Content $
  "#include <lua.h>\n"
  <> "#include <lauxlib.h>\n"
  <> "#include <lualib.h>\n"
  <> "\n"
  <> "#include <car.h>\n"
  <> "#include <robottools.h>\n"
  <> "\n"
  <> "/** top-level functions **/\n"
  <> "\n"
  <> "int tl_RtTrackSideTgAngleL(lua_State *L);\n"
  <> "\n"
  <> "const luaL_Reg tl_functions[] = {\n"
  <> "  {\"RtTrackSideTgAngleL\", tl_RtTrackSideTgAngleL},\n"
  <> "  {NULL, NULL}\n"
  <> "};\n"
  <> "\n"
  <> "/** Struct dispatch **/"

cppTop :: Content Cpp
cppTop = Content $
  "#include \"dispatch.h\"\n"
  <> "\n"
  <> "/** Generic helpers **/\n"
  <> "\n"
  <> "static void missingFieldError(lua_State *L, const char *field) {\n"
  <> "  const char *desc = \"No such field: \";\n"
  <> "  char error[strlen(desc) + strlen(field) + 1];\n"
  <> "  sprintf(error, \"%s%s\", desc, field);\n"
  <> "\n"
  <> "  lua_pushstring(L, error);\n"
  <> "  lua_error(L);\n"
  <> "}\n"
  <> "\n"
  <> "/** Top-level functions **/\n"
  <> "int tl_RtTrackSideTgAngleL(lua_State *L) {\n"
  <> "  tl_TrkLocPos *wrapper = (tl_TrkLocPos*) luaL_checkudata(L, 1, \"torcs.TrkLocPos\");\n"
  <> "  tTrkLocPos *pos = wrapper->wrapped;\n"
  <> "  tdble res = RtTrackSideTgAngleL(pos);\n"
  <> "  lua_pushnumber(L, res);\n"
  <> "  return 1;\n"
  <> "}"

genDispatchers :: [Dispatcher] -> Content Header
genDispatchers ds = dispatchSigs <.> dispatchTable
  where dispatchSigs = Content $ mconcat . (intersperse "\n") $ map dFunctionSig ds
        dispatchTable = Content $ "const luaL_Reg dispatchers[] = {\n"
                        <> mconcat (map ((<> ",\n") . ("  " <>) . dispatch) ds)
                        <> "  {NULL, NULL}\n};"
        dispatch (Dispatcher _ tName fName) = "{\"" <> tName <> "\", "
                                              <> fName <> "}"

codegen :: Structs -> CodeGen
codegen (Structs ss) = mconcat (map genStruct ss)

genStruct :: StructGen -> CodeGen
genStruct sg@(SG name fields') = CodeGen header cpp [disp]
  where fields = filter (not . fgIgnore) fields'
        header = headerContent (sg { sgFields = fields })
        cpp = cppContent (sg { sgFields = fields })
        disp = Dispatcher fSig (metatable name) fName
        fName = dispatchFunction name
        fSig = luaCFunctionSig fName

metatable :: StructName -> Text
metatable (StructName name) = "torcs." <> name

dispatchFunction :: StructName -> Text
dispatchFunction (StructName name) = "dispatch_" <> name

luaCFunctionSig :: Text -> Text
luaCFunctionSig name = "int " <> name <> "(lua_State *L);"

headerContent :: StructGen -> Content Header
headerContent (SG name fields) = comment <.> wrapper <.> fieldSigs
                                 <.> getterF <.> getterEntry <.> getterDispatch
  where comment = Content $ "/** " <> torcsStruct name <> " **/"
        wrapper = structWrapperDef name
        fieldSigs = Content $ mconcat . (intersperse "\n") $ map (genFieldSig name) fields
        getterF = getterFunctionDef name
        getterEntry = getterEntryDef name
        getterDispatch = fieldTable name fields

torcsStruct :: StructName -> Text
torcsStruct (StructName name) = "t" <> name

structWrapper :: StructName -> Text
structWrapper (StructName name) = "tl_" <> name

getterEntryType :: StructName -> Text
getterEntryType (StructName name) = "getterEntry_" <> name

structWrapperDef :: StructName -> Content Header
structWrapperDef sn = Content $
  flip renderPairs [("struct", torcsStruct sn), ("wrapper", structWrapper sn)] . template $
  "typedef struct {\n"
  <> "  $struct *wrapped;\n"
  <> "} $wrapper;"

getterFunctionDef :: StructName -> Content Header
getterFunctionDef sn@(StructName name) = Content $
  flip renderPairs [("struct", name), ("torcsStruct", torcsStruct sn)] . template $
  "typedef int (*getter_$struct) (lua_State *L, $torcsStruct *wrapped);"

getterFunctionName :: StructName -> FieldGen -> Text
getterFunctionName (StructName name) field = "f_" <> name <> "_" <> fgName field

genFieldSig :: StructName -> FieldGen -> Text
genFieldSig sn@(StructName name) field = "int " <> getterFunctionName sn field
                                     <> "(lua_State *L, t" <> name <> " *wrapped);"

getterEntryDef :: StructName -> Content Header
getterEntryDef sn@(StructName name) = Content $
  flip renderPairs [("struct", name), ("type", getterEntryType sn)] . template $
  "typedef struct {\n"
  <> "  const char *name;\n"
  <> "  getter_$struct getter;\n"
  <> "} $type;"

fieldTableName :: StructName -> Text
fieldTableName (StructName name) = "fields_" <> name

quote :: Text -> Text
quote t = "\"" <> t <> "\""

fieldTableEntry :: StructName -> FieldGen -> Text
fieldTableEntry sn field = let fName = fgName field
                           in "{" <> quote fName <> ", " <> getterFunctionName sn field <> "}"

indent :: Text -> Text
indent t = "  " <> t

fieldTable :: StructName -> [FieldGen] -> Content Header
fieldTable sn fields = Content $
  tableType <> " = {\n"
  <> entries <> ",\n"
  <> indent "{NULL, NULL}\n"
  <> "};"

  where tableType = "const " <> getterEntryType sn <> " " <> fieldTableName sn <> "[]"
        entries = (mconcat . (intersperse ",\n") $ map (indent . fieldTableEntry sn) fields)

cppContent :: StructGen -> Content Cpp
cppContent (SG name fields) = comment <.> dispatch <.> getters
  where comment = Content $ "/** " <> torcsStruct name <> " **/"
        dispatch = dispatcher name
        getters = mconcat $ map (genGetter name) fields

genGetter :: StructName -> FieldGen -> Content Cpp
genGetter sn field = let fname = fgName field in
  case fgType field of
   Prim p -> getter sn fname p
   Struct s -> structGetter sn fname s
   PrimArray p l -> arrayGetter sn fname (toCType p) ((T.pack . show) p) l
   StructArray s l -> structArrayGetter sn fname s l
   StructPointer s -> structGetterP sn fname (StructName s)

toCType :: Primitive -> Text
toCType p = case p of
  I -> "int"
  N -> "tdble"
  S -> "string"
  B -> "bool"

context :: [(Text, Text)] -> Context
context assoc x = maybe err id . lookup x $ assoc
  where err = error $ "No such field in template: " <> (show x)

renderPairs :: Template -> [(Text, Text)] -> Text
renderPairs t a = T.toStrict $ render t (context a)

dispatcher :: StructName -> Content Cpp
dispatcher (StructName struct) = Content $
  renderPairs dispatcherTemplate [("struct", struct)]

dispatcherTemplate :: Template
dispatcherTemplate = template $
  "int dispatch_$struct(lua_State *L) {\n"
  <> "  tl_$struct *wrapper = (tl_$struct *) luaL_checkudata(L, 1, \"torcs.$struct\");\n"
  <> "  luaL_argcheck(L, wrapper != NULL, 1, \"Expected tl_$struct\");\n"
  <> "\n"
  <> "  t$struct *wrapped = wrapper->wrapped;\n"
  <> "  const char *field = luaL_checkstring(L, 2);\n"
  <> "\n"
  <> "  getter_$struct f = NULL;\n"
  <> "  int i = 0;\n"
  <> "  while (fields_$struct[i].name != NULL && strcmp(fields_$struct[i].name, field) != 0) {\n"
  <> "    i++;\n"
  <> "  }\n"
  <> "\n"
  <> "  f = fields_$struct[i].getter;\n"
  <> "  if (f != NULL) {\n"
  <> "    return f(L, wrapped);\n"
  <> "  } else {\n"
  <> "    missingFieldError(L, field); // Doesn't return\n"
  <> "    return -1;\n"
  <> "  }\n"
  <> "}"

getter :: StructName -> Text -> Primitive -> Content Cpp
getter (StructName name) fname prim = Content $ renderPairs getterTemplate
                                       [("struct", name),
                                        ("fname", fname),
                                        ("ltype", luaType)]
  where luaType = (T.pack .show) prim

getterTemplate :: Template
getterTemplate = template $
  "int f_${struct}_${fname}(lua_State *L, t$struct *s) {\n"
  <> "  lua_push${ltype}(L, s->$fname);\n"
  <> "  return 1;\n"
  <> "}"

structGetter :: StructName -> Text -> StructName -> Content Cpp
structGetter (StructName outer) fname (StructName inner) = Content $
  renderPairs structGetterTemplate [("struct", outer),
                                    ("fname", fname),
                                    ("ftype", inner)]

structGetterTemplate :: Template
structGetterTemplate = template $
  "int f_${struct}_${fname}(lua_State *L, t$struct *s) {\n"
  <> "  tl_$ftype *wrapper = (tl_$ftype *) lua_newuserdata(L, sizeof(tl_$ftype));\n"
  <> "  wrapper->wrapped = &(s->$fname);\n"
  <> "\n"
  <> "  luaL_getmetatable(L, \"torcs.$ftype\");\n"
  <> "  lua_setmetatable(L, -2);\n"
  <> "  return 1;\n"
  <> "}"

structGetterP :: StructName -> Text -> StructName -> Content Cpp
structGetterP (StructName outer) fname (StructName inner) = Content $
  renderPairs structGetterPTemplate [("struct", outer),
                                     ("fname", fname),
                                     ("ftype", inner)]

structGetterPTemplate :: Template
structGetterPTemplate = template $
  "int f_${struct}_${fname}(lua_State *L, t$struct *s) {\n"
  <> "  tl_${ftype} *wrapper = (tl_${ftype} *) lua_newuserdata(L, sizeof(tl_${ftype}));\n"
  <> "  wrapper->wrapped = s->${fname};\n"
  <> "\n"
  <> "  luaL_getmetatable(L, \"torcs.${ftype}\");\n"
  <> "  lua_setmetatable(L, -2);\n"
  <> "  return 1;\n"
  <> "}"

arrayGetter :: StructName -> Text -> Text -> Text -> Int -> Content Cpp
arrayGetter (StructName struct) fname ctype ltype len = Content $
  renderPairs arrayGetterTemplate [("struct", struct),
                                   ("fname", fname),
                                   ("ctype", ctype),
                                   ("ltype", ltype),
                                   ("length", (T.pack . show) len)]

arrayGetterTemplate :: Template
arrayGetterTemplate = template $
  "int f_${struct}_${fname}(lua_State *L, t$struct *s) {\n"
  <> "  int length = $length;\n"
  <> "  $ctype *array = s->$fname;\n"
  <> "\n"
  <> "  lua_createtable(L, length, 0);\n"
  <> "\n"
  <> "  int i;\n"
  <> "  for (i = 0; i < length; i++) {\n"
  <> "    lua_pushinteger(L, i);\n"
  <> "    lua_push${ltype}(L, array[i]);\n"
  <> "    lua_settable(L, -3);\n"
  <> "  }\n"
  <> "\n"
  <> "  return 1;\n"
  <> "}"

structArrayGetter :: StructName -> Text -> Text -> Int -> Content Cpp
structArrayGetter (StructName struct) fname ftype len = Content $
  renderPairs structArrayGetterTemplate [("struct", struct),
                                         ("fname", fname),
                                         ("ftype", ftype),
                                         ("length", (T.pack . show) len)]

structArrayGetterTemplate :: Template
structArrayGetterTemplate = template $
  "int f_${struct}_${fname}(lua_State *L, t$struct *s) {\n"
  <> "  int length = $length;\n"
  <> "  t$ftype *array = s->$fname;\n"
  <> "\n"
  <> "  lua_createtable(L, length, 0);\n"
  <> "\n"
  <> "  int i;\n"
  <> "  for (i = 0; i < length; i++) {\n"
  <> "    lua_pushinteger(L, i);\n"
  <> "\n"
  <> "    tl_$ftype *wrapper = (tl_$ftype *) lua_newuserdata(L, sizeof(tl_$ftype));\n"
  <> "    wrapper->wrapped = &(array[i]);\n"
  <> "\n"
  <> "    luaL_getmetatable(L, \"torcs.$ftype\");\n"
  <> "    lua_setmetatable(L, -2);\n"
  <> "\n"
  <> "    lua_settable(L, -3);\n"
  <> "  }\n"
  <> "\n"
  <> "  return 1;\n"
  <> "}"

{- JSON model parsing -}

instance FromJSON Structs where
  parseJSON (Object o) = Structs <$> o .: "structs"
  parseJSON _ = fail "Error parsing Structs"

instance FromJSON StructGen where
  parseJSON (Object o) = SG <$> o .: "name" <*> o .: "fields"
  parseJSON _ = fail "Error parsing StructGen"

instance FromJSON StructName where
  parseJSON t = StructName <$> parseJSON t

instance FromJSON FieldGen where
  parseJSON (Object o) = FG <$> o .: "name" <*> parseGenType o <*> o .:? "ignore" .!= False
  parseJSON _ = fail "Error parsing FieldGen"

parseGenType :: Object -> Parser GenType
parseGenType o = do
  typ <- o .: "type"
  case typ of
   "integer" -> return $ Prim I
   "number" -> return $ Prim N
   "string" -> return $ Prim S
   "boolean" -> return $ Prim B
   "array" -> parseArrayGen o
   "pointer" -> StructPointer <$> o .: "targetType"
   str -> return $ Struct (StructName str)

parseArrayGen :: Object -> Parser GenType
parseArrayGen o = do
  len <- o .: "length"
  elemType <- o .: "elemType"
  return $ case elemType of
   "integer" -> PrimArray I len
   "number" -> PrimArray N len
   "string" -> PrimArray S len
   "boolean" -> PrimArray B len
   str -> StructArray str len

parsePointerGen :: Object -> Parser GenType
parsePointerGen o = StructPointer <$> o .: "targetType"
