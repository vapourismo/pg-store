{-# LANGUAGE TemplateHaskell #-}

module Database.PostgreSQL.Store.TH (makeTable) where

import Data.List
import Data.String
import Language.Haskell.TH
import Database.PostgreSQL.Store.Types

-- |
sanitizeName :: Name -> String
sanitizeName name =
	"\"" ++ show name ++ "\""

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
			")"

		columns =
			map sanitizeName fields

		values =
			map (\ idx -> "$" ++ show idx) [1 .. length fields]

-- | Generate the update statement for a table.
updateStatementE :: Name -> Int -> Q Exp
updateStatementE name numFields =
	[e| fromString $(stringE statement) |]
	where
		statement =
			"UPDATE " ++ sanitizeName name ++
			" SET " ++ intercalate ", " values ++
			" WHERE id = $1"

		values =
			map (\ idx -> "$" ++ show idx) [2 .. numFields + 1]

-- | Generate an expression which gathers all records from a type and packs them into a list.
-- `packParamsE 'row ['field1, 'field2]` generates `[pack (field1 row), pack (field2 row)]`
packParamsE :: Name -> [Name] -> Q Exp
packParamsE row fields =
	ListE <$> mapM extract fields
	where
		extract name =
			[e| pack ($(varE name) $(varE row)) |]

-- |
implementTable :: Name -> Name -> [(Name, Type)] -> Q [Dec]
implementTable table _ctor fields =
	[d| instance Table $(pure (ConT table)) where
	        insert row =
	            Statement {
	                statementContent = $(insertStatementE table fieldNames),
	                statementParams  = $(packParamsE 'row fieldNames)
	            }

	        update rid row =
	            Statement {
	                statementContent = $(updateStatementE table (length fieldNames)),
	                statementParams  = pack rid : $(packParamsE 'row fieldNames)
	            } |]
	where
		fieldNames = map fst fields

-- |
makeTable :: Name -> Q [Dec]
makeTable name = do
	info <- reify name
	case info of
		TyConI (DataD [] _ [] [RecC ctor records] _) | length records > 0 ->
			implementTable name ctor (map (\ (fn, _, ft) -> (fn, ft)) records)

		_ -> fail "Need type-constructor for a context-less type-variable-free data type with only one record constructor and 1 or more records"

