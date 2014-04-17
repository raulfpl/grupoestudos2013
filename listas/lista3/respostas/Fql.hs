module Fql where
import Data.List

-- Definindo uma tabela

data Table = Table {
                tableName   :: String,        -- nome da tabela
                tableFields :: [Field],       -- campos
                values :: [[String]]          -- valores associados a cada campo
           } deriving (Eq, Ord)

-- Definição de Campos

data Field = Field  {
                fieldName :: String,          -- nome do campo
                fieldType :: Type             -- tipo de dados associado com este campo
           }
           deriving (Eq, Ord, Show)

-- Definição dos tipos de campos

data Type = TyInt                             -- números inteiros
          | TyDouble                          -- números de ponto flutuante
          | TyBool                            -- valores lógicos
          | TyVarChar (Maybe Int)             -- Strings.Pode especificar o comp. máx. n, por Just n. Nothing => 255
          | TyDate                            -- Datas
          | TyCurrency                        -- valores monetários
          deriving (Eq, Ord)


-- Definindo tabelas de exemplo

client :: Table
client = Table "Cliente" fieldsCliente dataCliente

fieldsCliente :: [Field]
fieldsCliente = [Field "id" TyInt, Field "nome" (TyVarChar (Just 15)), Field "cpf" (TyVarChar (Just 11))]

dataCliente :: [[String]]
dataCliente = [["1", "Jose da Silva", "23333245678"], ["2", "Joaquim Souza", "09863737213"],
               ["3", "Roberto Martins", "45627819081"]]


address :: Table
address = Table "Endereco" fieldsEndereco dataEndereco

fieldsEndereco :: [Field]
fieldsEndereco = [Field "cpf" (TyVarChar (Just 11)),
                  Field "rua" (TyVarChar (Just 15)),
                  Field "bairro" (TyVarChar (Just 15)),
                  Field "cidade" (TyVarChar (Just 15)),
                  Field "estado" (TyVarChar (Just 2))]

dataEndereco :: [[String]]
dataEndereco = [["23333245678", "rua 2", "alfa", "jurema do sul", "SC"],
                ["09863737213", "rua 89", "beta", "jurema do norte", "SC"],
                ["45627819081", "rua 10", "gama", "jurema do leste", "SC"]]

-----------------------------------------------------------------------------------------------------------------
-- Exercício 1 : imprimir o esquema da tabela
-----------------------------------------------------------------------------------------------------------------

data Schema = Type :*: Schema
            | Nil
            deriving (Eq, Ord)


instance Show Type where
    show TyInt                = "Int"
    show TyDouble             = "Double"
    show TyBool               = "Bool"
    show (TyVarChar Nothing)  = "VarChar"
    show (TyVarChar (Just n)) = "VarChar[" ++ (show n) ++ "]"
    show TyDate               = "Date"
    show TyCurrency           = "Currency"

instance Show Schema where
    show Nil = ""
    show (t :*: s) = show t ++ (if s /= Nil then " X " else "") ++ show s

schema :: Table -> Schema
schema (Table _ fields _) = schemaFromFields fields
    where schemaFromFields []                = Nil
          schemaFromFields ((Field _ ft):fs) = ft :*: schemaFromFields fs


-------------------------------------------------------------------------------------------------------------
-- Exercício 2 : imprimir a tabela
-------------------------------------------------------------------------------------------------------------


instance Show Table where
    show t = tableName t ++ ":\n" ++ separator ++ header ++ "\n" ++ separator ++ tableLines
        where separator      = (replicate (sum colLengths -1) '-') ++ "\n"
              colLengths     = [max (maximum (map length c)) (length f) + 1 | (c, f) <- zip cols names]
              cols           = transpose . values $ t
              names          = map fieldName (tableFields t)
              tableLines     = unlines newLines
              header         = concat $ zipWith putSpaces names colLengths
              newLines       = map ajustLine $ values t
              putSpaces v l  = v ++ replicate (l - (length v)) ' '
              ajustLine line = concat $ zipWith putSpaces line colLengths


-------------------------------------------------------------------------------
-- Exercício 3: contar o número de registros de uma tabela
-------------------------------------------------------------------------------


count :: Table -> Int
count (Table _ _ ls) = length ls

-------------------------------------------------------------------------------
-- Exercício 4: projeção
-------------------------------------------------------------------------------

project :: Table -> [String] -> Either String Table
project t q = if (length newFields == length q) then Right (Table newName newFields newValues) else Left projectError
    where (newFields, newCols) = unzip $ filter ((`elem` q) . fieldName . fst) (zip (tableFields t) cols)
          cols                 = transpose . values $ t
          newValues            = transpose newCols
          newName              = tableName t ++ "-" ++ intercalate "-" q
          projectError         = "The fields:\n" ++ unlines (filter (not . (`elem` (map fieldName $ tableFields t))) q)
                                    ++ "aren't defined in table: " ++ tableName t

-------------------------------------------------------------------------------
-- Exercício 5: Restrição
-------------------------------------------------------------------------------

data Condition = Condition {
                    field :: String,
                    condition :: String -> Bool
               }


restrict :: Table -> Condition -> Either String Table
restrict t c = if ((field c) `elem` fieldNames) then Right (Table (tableName t) fields newValues) else Left "The field isn't defined in the table"
    where newValues    = filter lineMatch vs
          vs           = values t
          fields       = tableFields t
          fieldNames   = map fieldName fields
          matchedField = head $ filter (\f -> f == field c) fieldNames
          lineMatch l  = (condition c) $ l !! fi
          Just fi      = elemIndex (field c) fieldNames

cond :: Condition
cond = Condition "nome" (\s -> head s == 'R')


-------------------------------------------------------------------------------
-- Exercício 6: Junção
-------------------------------------------------------------------------------

join :: Table -> Table -> Either String Table
join t1 t2 = undefined


-------------------------------------------------------------------------------
-- Exercício 7: Verificação de esquema
-------------------------------------------------------------------------------

-- definição de uma tabela tipada

data TypedTable = TypedTable {
                    tyTableName   :: String,
                    tyTableSchema :: Schema,
                    typedValues   :: [[Value]]
                } deriving (Eq, Ord)

-- valores tipados

data Value = IntVal Int
           | DoubleVal Double
           | BoolVal Bool
           | VarCharVal String
           | DateVal Date
           | CurrencyVal Currency
           deriving (Eq, Ord)

-- tipos de dados auxiliares

data Date = Date {
              day   :: Int,           -- dia
              month :: Int,           -- mes
              year  :: Int            -- ano
          } deriving (Eq, Ord)

newtype Currency = Currency Double   -- valores monetários
                   deriving (Eq, Ord)

typedTable :: Table -> Either String TypedTable
typedTable = undefined