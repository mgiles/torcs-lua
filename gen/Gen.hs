{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative ((<$>), (<*>))
import Data.Aeson (eitherDecode)
import Data.Aeson.Types
import qualified Data.ByteString.Lazy as B
import Data.List (intersperse)
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T (pack, replace)
import qualified Data.Text.IO as T (writeFile)
import System.Environment (getArgs)

newtype Structs = Structs { structs :: [StructGen]}
             deriving (Show)

data StructGen = SG {
  sgName :: Text,
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
             | Struct Text
             | PrimArray Primitive Int
             | StructArray Text Int
             | StructPointer Text
             deriving (Show)

{- Code generation -}

load :: IO CodeGen
load = do
  Right ss <- B.readFile "../model.json" >>= return . eitherDecode
  return (codegen ss)

main :: IO ()
main = do
  [model, header, cpp] <- getArgs
  Right ss <- B.readFile model >>= return . eitherDecode
  let gen = codegen ss
      cppText = cppTop <.> forCpp gen
  T.writeFile cpp cppText

  let headerText = headerTop <.> (genDispatchers . forDispatchers) gen <.> forH gen
  T.writeFile header headerText

headerTop :: Text
headerTop =
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
  <> "};"

cppTop :: Text
cppTop =
  "#include \"dispatch.h\"\n"
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
  <> "}\n"

data CodeGen = CodeGen {
  forH :: Text,
  forCpp :: Text,
  forDispatchers :: [Dispatcher]
  } deriving (Show)

data Dispatcher = Dispatcher {
  dFunctionSig :: Text,
  dTableName :: Text,
  dFunctionName :: Text
  } deriving (Show)

genDispatchers :: [Dispatcher] -> Text
genDispatchers ds = dispatchSigs <.> dispatchTable
  where dispatchSigs = mconcat . (intersperse "\n") $ map dFunctionSig ds
        dispatchTable = "const luaL_Reg dispatchers[] = {\n"
                        <> mconcat (map ((<> ",\n") . ("  " <>) . dispatch) ds)
                        <> "  {NULL, NULL}\n};"
        dispatch (Dispatcher _ tName fName) = "{\"" <> tName <> "\", "
                                              <> fName <> "}"

instance Monoid CodeGen where
  mempty = CodeGen "" "" []
  (CodeGen h c d) `mappend` (CodeGen h' c' d') = CodeGen (h <> "\n\n" <> h')
                                                         (c <> "\n\n" <> c')
                                                         (d <> d')

(<.>) :: Text -> Text -> Text
t1 <.> t2 = t1 <> "\n\n" <> t2

codegen :: Structs -> CodeGen
codegen (Structs ss) = mconcat (map genStruct ss)

genStruct :: StructGen -> CodeGen
genStruct sg@(SG name fields') = CodeGen header cpp [disp]
  where fields = filter (not . fgIgnore) fields'
        header = headerContent (sg { sgFields = fields })
        cpp = cppContent (sg { sgFields = fields })
        disp = Dispatcher fSig ("torcs." <> name) fName
        fName = "dispatch_" <> name
        fSig = "int " <> fName <> "(lua_State *L);"

headerContent :: StructGen -> Text
headerContent (SG name fields) = comment <.> wrapper <.> fieldSigs
                                 <.> getterF <.> getterEntry <.> getterDispatch
  where comment = "/** t" <> name <> " **/"
        wrapper = "typedef struct {\n  t" <> name <> " *wrapped;\n} tl_" <> name <> ";"
        fieldSigs = mconcat . (intersperse "\n") $ map (genFieldSig name) fields
        getterF = "typedef int (*getter_" <> name <> ") (lua_State *L, t" <> name <> " *wrapped);"
        getterEntry = "typedef struct {\n  const char *name;\n  getter_" <> name <> " getter;\n"
                      <> "} getterEntry_" <> name <> ";"
        getterDispatch = "const getterEntry_" <> name <> " fields_" <> name <> "[] = {\n"
                         <> (mconcat . (intersperse ",\n") $ map (("  " <>) . entry) fields)
                         <> ",\n  {NULL, NULL}\n};"
        entry f = "{\"" <> fgName f <> "\", f_" <> name <> "_" <> fgName f <> "}"

cppContent :: StructGen -> Text
cppContent (SG name fields) = comment <.> dispatch <.> getters
  where comment = "/** t" <> name <> " **/"
        dispatch = dispatcher name
        getters = mconcat . intersperse "\n\n" $ map (genGetter name) fields

genGetter :: Text -> FieldGen -> Text
genGetter struct field = let fname = fgName field in
  case fgType field of
   Prim p -> getter struct fname ((T.pack . show) p)
   Struct s -> structGetter struct fname s
   PrimArray p l -> arrayGetter struct fname (toCType p) ((T.pack . show) p) l
   StructArray s l -> structArrayGetter struct fname s l
   StructPointer s -> structGetterP struct fname s

toCType :: Primitive -> Text
toCType p = case p of
  I -> "int"
  N -> "tdble"
  S -> "string"
  B -> "bool"

dispatcher :: Text -> Text
dispatcher struct = T.replace "STRUCT" struct dispatcherTemplate

dispatcherTemplate :: Text
dispatcherTemplate =
  "int dispatch_STRUCT(lua_State *L) {\n"
  <> "  tl_STRUCT *wrapper = (tl_STRUCT *) luaL_checkudata(L, 1, \"torcs.STRUCT\");\n"
  <> "  luaL_argcheck(L, wrapper != NULL, 1, \"Expected tl_STRUCT\");\n"
  <> "\n"
  <> "  tSTRUCT *wrapped = wrapper->wrapped;\n"
  <> "  const char *field = luaL_checkstring(L, 2);\n"
  <> "\n"
  <> "  getter_STRUCT f = NULL;\n"
  <> "  int i = 0;\n"
  <> "  while (fields_STRUCT[i].name != NULL && strcmp(fields_STRUCT[i].name, field) != 0) {\n"
  <> "    i++;\n"
  <> "  }\n"
  <> "\n"
  <> "  f = fields_STRUCT[i].getter;\n"
  <> "  if (f != NULL) {\n"
  <> "    return f(L, wrapped);\n"
  <> "  } else {\n"
  <> "    missingFieldError(L, field); // Doesn't return\n"
  <> "    return -1;\n"
  <> "  }\n"
  <> "}"

getter :: Text -> Text -> Text -> Text
getter struct fname ltype =
  T.replace "STRUCT" struct $
  T.replace "FNAME" fname $
  T.replace "LTYPE" ltype $
  getterTemplate

getterTemplate :: Text
getterTemplate =
  "int f_STRUCT_FNAME(lua_State *L, tSTRUCT *s) {\n"
  <> "  lua_pushLTYPE(L, s->FNAME);\n"
  <> "  return 1;\n"
  <> "}"

structGetter :: Text -> Text -> Text -> Text
structGetter struct fname ftype =
  T.replace "STRUCT" struct $
  T.replace "FNAME" fname $
  T.replace "FTYPE" ftype $
  structGetterTemplate

structGetterTemplate :: Text
structGetterTemplate =
  "int f_STRUCT_FNAME(lua_State *L, tSTRUCT *s) {\n"
  <> "  tl_FTYPE *wrapper = (tl_FTYPE *) lua_newuserdata(L, sizeof(tl_FTYPE));\n"
  <> "  wrapper->wrapped = &(s->FNAME);\n"
  <> "\n"
  <> "  luaL_getmetatable(L, \"torcs.FTYPE\");\n"
  <> "  lua_setmetatable(L, -2);\n"
  <> "  return 1;\n"
  <> "}"

structGetterP :: Text -> Text -> Text -> Text
structGetterP struct fname ftype =
  T.replace "STRUCT" struct $
  T.replace "FNAME" fname $
  T.replace "FTYPE" ftype $
  structGetterPTemplate

structGetterPTemplate :: Text
structGetterPTemplate = T.replace "&(s->FNAME)" "s->FNAME" structGetterTemplate

arrayGetter :: Text -> Text -> Text -> Text -> Int -> Text
arrayGetter struct fname ctype ltype len =
  T.replace "STRUCT" struct $
  T.replace "FNAME" fname $
  T.replace "CTYPE" ctype $
  T.replace "LTYPE" ltype $
  T.replace "LENGTH" ((T.pack . show) len) $
  arrayGetterTemplate

arrayGetterTemplate :: Text
arrayGetterTemplate =
  "int f_STRUCT_FNAME(lua_State *L, tSTRUCT *s) {\n"
  <> "  int length = LENGTH;\n"
  <> "  CTYPE *array = s->FNAME;\n"
  <> "\n"
  <> "  lua_createtable(L, length, 0);\n"
  <> "\n"
  <> "  int i;\n"
  <> "  for (i = 0; i < length; i++) {\n"
  <> "    lua_pushinteger(L, i);\n"
  <> "    lua_pushLTYPE(L, array[i]);\n"
  <> "    lua_settable(L, -3);\n"
  <> "  }\n"
  <> "\n"
  <> "  return 1;\n"
  <> "}"

structArrayGetter :: Text -> Text -> Text -> Int -> Text
structArrayGetter struct fname ftype len =
  T.replace "STRUCT" struct $
  T.replace "FNAME" fname $
  T.replace "FTYPE" ftype $
  T.replace "LENGTH" ((T.pack . show) len) $
  structArrayGetterTemplate

structArrayGetterTemplate :: Text
structArrayGetterTemplate =
  "int f_STRUCT_FNAME(lua_State *L, tSTRUCT *s) {\n"
  <> "  int length = LENGTH;\n"
  <> "  tFTYPE *array = s->FNAME;\n"
  <> "\n"
  <> "  lua_createtable(L, length, 0);\n"
  <> "\n"
  <> "  int i;\n"
  <> "  for (i = 0; i < length; i++) {\n"
  <> "    lua_pushinteger(L, i);\n"
  <> "\n"
  <> "    tl_FTYPE *wrapper = (tl_FTYPE *) lua_newuserdata(L, sizeof(tl_FTYPE));\n"
  <> "    wrapper->wrapped = &(array[i]);\n"
  <> "\n"
  <> "    luaL_getmetatable(L, \"torcs.FTYPE\");\n"
  <> "    lua_setmetatable(L, -2);\n"
  <> "\n"
  <> "    lua_settable(L, -3);\n"
  <> "  }\n"
  <> "\n"
  <> "  return 1;\n"
  <> "}"


genFieldSig :: Text -> FieldGen -> Text
genFieldSig sName field = "int f_" <> sName <> "_" <> fgName field
                          <> "(lua_State *L, t" <> sName <> " *wrapped);"

{- JSON model parsing -}

instance FromJSON Structs where
  parseJSON (Object o) = Structs <$> o .: "structs"
  parseJSON _ = fail "Error parsing Structs"

instance FromJSON StructGen where
  parseJSON (Object o) = SG <$> o .: "name" <*> o .: "fields"
  parseJSON _ = fail "Error parsing StructGen"

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
   str -> return $ Struct str

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
