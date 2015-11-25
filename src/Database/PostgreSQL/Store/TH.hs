{-# LANGUAGE TemplateHaskell #-}

module Database.PostgreSQL.Store.TH (
	mkTable,
	mkCreateStatement,
	mkDropStatement
) where

import Data.List
import Data.String
import Data.Typeable
import Language.Haskell.TH
import Database.PostgreSQL.Store.Types

-- | Generate the sanitized representation of a name.
sanitizeName :: Name -> String
sanitizeName name =
	"\"" ++ show name ++ "\""

-- | Generate the name of the identifying field.
identField :: Name -> String
identField name =
	"\"" ++ maybe [] (++ ".") (nameModule name) ++ "$id\""

-- | Generate the insert statement for a table.
insertStatementE :: Name -> [Name] -> Q Exp
insertStatementE name fields =
	[e| fromString $(stringE statement) |]
	where
		statement =
			"INSERT INTO " ++ sanitizeName name ++ " (" ++
				intercalate ", " columns ++
			") VALUES (" ++
				intercalate ", " values ++
			") RETURNING " ++ identField name

		columns =
			map sanitizeName fields

		values =
			map (\ idx -> "$" ++ show idx) [1 .. length fields]

-- | Generate the empty update statement for a table.
emptyUpdateStatementE :: Name -> Q Exp
emptyUpdateStatementE name =
	[e| fromString $(stringE statement) |]
	where
		statement =
			"UPDATE " ++ sanitizeName name ++
			" SET " ++ identField name ++ " = $1" ++
			" WHERE " ++ identField name ++ " = $1"

-- | Generate the update statement for a table.
updateStatementE :: Name -> Int -> Q Exp
updateStatementE name numFields =
	[e| fromString $(stringE statement) |]
	where
		statement =
			"UPDATE " ++ sanitizeName name ++
			" SET " ++ intercalate ", " values ++
			" WHERE " ++ identField name ++ " = $1"

		values =
			map (\ idx -> "$" ++ show idx) [2 .. numFields + 1]

-- | Generate the delete statement for a table.
deleteStatementE :: Name -> Q Exp
deleteStatementE name =
	[e| fromString $(stringE statement) |]
	where
		statement = "DELETE FROM " ++ sanitizeName name ++ " WHERE " ++ identField name ++ " = $1"

-- | Generate the create statement for a table.
createStatementE :: Name -> [(Name, Type)] -> Q Exp
createStatementE name fields =
	[e| fromString ($(stringE statementBegin) ++
	                intercalate ", " ($(stringE anchorDescription) : $(descriptions)) ++
	                $(stringE statementEnd)) |]
	where
		statementBegin = "CREATE TABLE IF NOT EXISTS " ++ sanitizeName name ++ " ("
		statementEnd = ")"

		anchorDescription =
			identField name ++ " BIGSERIAL NOT NULL PRIMARY KEY"

		descriptions =
			ListE <$> mapM describeField fields

		describeField (fname, ftype) =
			[e| $(stringE (sanitizeName fname)) ++ " " ++
			    show (columnTypeDescription :: ColumnTypeDescription $(pure ftype)) |]

-- | Generate the drop statement for a table.
dropStatementE :: Name -> Q Exp
dropStatementE name =
	[e| fromString $(stringE statement) |]
	where
		statement = "DROP TABLE IF EXISTS " ++ sanitizeName name

-- | Generate an expression which gathers all records from a type and packs them into a list.
-- `packParamsE 'row ['field1, 'field2]` generates `[pack (field1 row), pack (field2 row)]`
packParamsE :: Name -> [Name] -> Q Exp
packParamsE row fields =
	ListE <$> mapM extract fields
	where
		extract name =
			[e| pack ($(varE name) $(varE row)) |]

-- | Implement an instance 'Table' for the given type.
implementTable :: Name -> Name -> [(Name, Type)] -> Q [Dec]
implementTable table _ctor fields =
	[d| instance Table $(pure (ConT table)) where
	        insertStatement row =
	            Statement {
	                statementContent = $(insertStatementE table fieldNames),
	                statementParams  = $(packParamsE 'row fieldNames)
	            }

	        updateStatement (Reference rid) =
	        	Statement {
	                statementContent = $(emptyUpdateStatementE table),
	                statementParams  = [pack rid]
	            }
	        updateStatement (Resolved rid row) =
	            Statement {
	                statementContent = $(updateStatementE table (length fieldNames)),
	                statementParams  = pack rid : $(packParamsE 'row fieldNames)
	            }

	        deleteStatement ref =
	            Statement {
	                statementContent = $(deleteStatementE table),
	                statementParams  = [pack (referenceID ref)]
	            }

	        createStatement _ =
	            Statement {
	                statementContent = $(createStatementE table fields),
	                statementParams  = []
	            }

	        dropStatement _ =
	            Statement {
	                statementContent = $(dropStatementE table),
	                statementParams  = []
	            } |]
	where
		fieldNames = map fst fields

-- | Make a type ready to be used as a table.
mkTable :: Name -> Q [Dec]
mkTable name = do
	info <- reify name
	case info of
		TyConI (DataD [] _ [] [RecC ctor records] _) | length records > 0 ->
			implementTable name ctor fields
			where
				fields = map (\ (fn, _, ft) -> (fn, ft)) records

		_ -> fail "Need type-constructor for a context-less type-variable-free data type with only one record constructor and 1 or more records"

-- | Inline the create table statement of a table.
mkCreateStatement :: Name -> Q Exp
mkCreateStatement name =
	[e| createStatement (Proxy :: Proxy $(pure (ConT name))) |]

-- | Inline the drop table statement of a table.
mkDropStatement :: Name -> Q Exp
mkDropStatement name =
	[e| dropStatement (Proxy :: Proxy $(pure (ConT name))) |]
