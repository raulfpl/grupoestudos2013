module Fql where

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
	show = undefined
	
instance Show Schema where
	show = undefined
		
schema :: Table -> Schema
schema = undefined 


-------------------------------------------------------------------------------------------------------------
-- Exercício 2 : imprimir a tabela
-------------------------------------------------------------------------------------------------------------


instance Show Table where
    show = undefined   


-------------------------------------------------------------------------------              
-- Exercício 3: contar o número de registros de uma tabela
-------------------------------------------------------------------------------


count :: Table -> Int
count = undefined

-------------------------------------------------------------------------------
-- Exercício 4: projeção
-------------------------------------------------------------------------------

project :: Table -> [String] -> Either String Table
project = undefined

-------------------------------------------------------------------------------
-- Exercício 5: Restrição
-------------------------------------------------------------------------------

data Condition = Condition {
					field :: String,
					condition :: String -> Bool
               }
               

restrict :: Table -> Condition -> Either String Table
restrict t c = undefined

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
