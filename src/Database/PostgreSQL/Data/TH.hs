{-# LANGUAGE TemplateHaskell #-}

module Database.PostgreSQL.Data.TH where

import Data.String
import Language.Haskell.TH
import Database.PostgreSQL.Data.Types
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.ToField

-- | Generate a 'ColumnDescription' expression for a column name and type.
describeColumnE :: Name -> Type -> Q Exp
describeColumnE name t =
	[e| ColumnDescription (fromString $(columnName)) (describeColumnType :: ColumnTypeDescription $(pure t)) |]
	where
		columnName = stringE (nameBase name)

-- | Generate a 'TableDescription' for a table name and its columns.
describeTableE :: Name -> [(Name, Type)] -> Q Exp
describeTableE name fields =
	[e| TableDescription (fromString $(tableName)) $(tableFields) |]
	where
		tableName = stringE (show name)
		tableFields = ListE <$> mapM (uncurry describeColumnE) fields

-- |
extractTableE :: Name -> [(Name, Type)] -> Q Exp
extractTableE rowName fields =
	ListE <$> mapM makeField fields
	where
		makeField (n, _) = [e| toField ($(pure (VarE n)) $(pure (VarE rowName))) |]

-- | Implement an instance of 'Table' for a given data type.
makeTable :: Name -> Q [Dec]
makeTable name = do
	info <- reify name
	case info of
		TyConI (DataD [] _ [] [RecC _ records] _) ->
			[d| instance ToRow $(pure (ConT name)) where
			        toRow $(pure (VarP rowName)) = $(extractTableE rowName fields)

			    instance Table $(pure (ConT name)) where
			        describeTable = $(describeTableE name fields) |]
			where
				fields = map (\ (fn, _, ft) -> (fn, ft)) records
				rowName = mkName "row"

		_ -> fail "Need type-constructor for a context-less type-variable-free data type with only one record constructor"

