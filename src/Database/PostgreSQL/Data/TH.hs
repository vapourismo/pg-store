{-# LANGUAGE TemplateHaskell #-}

module Database.PostgreSQL.Data.TH where

import Data.Typeable
import Language.Haskell.TH
import Database.PostgreSQL.Data.Types

-- | Generate a 'ColumnDescription' expression for a column name and type.
describeColumnE :: Name -> Type -> Q Exp
describeColumnE name t =
	[e| ColumnDescription $(columnName) (describeColumnType $(columnTypeProxy)) |]
	where
		columnName = stringE (nameBase name)
		columnTypeProxy = [e| Proxy :: Proxy $(pure t) |]

-- | Generate a 'TableDescription' for a table name and its columns.
describeTableE :: Name -> [(Name, Type)] -> Q Exp
describeTableE name fields =
	[e| TableDescription $(tableName) $(tableFields) |]
	where
		tableName = stringE (show name)
		tableFields = ListE <$> mapM (uncurry describeColumnE) fields

-- | Implement an instance of 'Table' for a given data type.
makeTable :: Name -> Q [Dec]
makeTable name = do
	info <- reify name
	case info of
		TyConI (DataD [] _ [] [RecC _ records] _) ->
			[d| instance Table $(pure (ConT name)) where
			        describeTable _ = $(describeTableE name fields) |]
			where
				fields = map (\ (fn, _, ft) -> (fn, ft)) records

		_ -> fail "Need type-constructor for a context-less type-variable-free data type with only one record constructor"

