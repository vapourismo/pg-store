{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, RecordWildCards #-}

-- |
-- Module:     Database.PostgreSQL.Store.Table
-- Copyright:  (c) Ole Krüger 2016
-- License:    BSD3
-- Maintainer: Ole Krüger <ole@vprsm.de>
module Database.PostgreSQL.Store.Table (
	-- * Table information
	TableConstraint (..),
	TableInformation (..),
	Table (..),

	-- * Template Haskell
	TableOptions (..),
	defaultTableOptions,
	InterimTableConstraint (..),
	makeTable,

	createTable,
	createTable_
) where

import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax

import           Control.Monad

import           Data.Proxy
import qualified Data.ByteString                    as B
import qualified Blaze.ByteString.Builder           as B
import qualified Blaze.ByteString.Builder.Char.Utf8 as B

import           Database.PostgreSQL.Store.Errand
import           Database.PostgreSQL.Store.Result
import           Database.PostgreSQL.Store.Columns
import           Database.PostgreSQL.Store.Utilities
import           Database.PostgreSQL.Store.Table.Class
import           Database.PostgreSQL.Store.Query.Builder
import           Database.PostgreSQL.Store.Query

-- | Table field declaration
data TableField = TableField Name Type

-- | Table type declaration
data TableDec = TableDec Name Name [TableField]

-- | Check that each field's type has an implementation of 'Column'.
checkRecordFields :: [VarBangType] -> Q [TableField]
checkRecordFields fields =
	forM fields $ \ (name, _, typ) ->
		TableField name typ <$ do
			ii <- isInstance ''Column [typ]
			unless ii $
				reportWarning ("Type of field '"
				               ++ show name
				               ++ "' ('"
				               ++ show typ
				               ++ "') type does not implement '"
				               ++ show ''Column
				               ++ "'")

-- | Check that each constructor parameter type implements 'Column'.
checkNormalFields :: [BangType] -> Q [TableField]
checkNormalFields fields =
	forM (fields `zip` [1 .. length fields]) $ \ ((_, typ), idx) -> do
		ii <- isInstance ''Column [typ]
		unless ii $
			reportWarning ("Type of constructor parameter #"
			               ++ show idx
			               ++ " ('"
			               ++ show typ
			               ++ "') type does not implement '"
			               ++ show ''Column
			               ++ "'")

		flip TableField typ <$> newName ("column" ++ show idx)

-- | Verify that the given constructor is viable and construct a 'TableDec'.
checkTableCtor :: Name -> Con -> Q TableDec
checkTableCtor typeName (RecC ctorName ctorFields) = do
	when (length ctorFields < 1)
	     (fail ("'" ++ show ctorName ++ "' must have at least one field"))

	fields <- checkRecordFields ctorFields
	pure (TableDec typeName ctorName fields)

checkTableCtor typeName (NormalC ctorName ctorFields) = do
	when (length ctorFields < 1)
	     (fail ("'" ++ show ctorName ++ "' must have at least one field"))

	fields <- checkNormalFields ctorFields
	pure (TableDec typeName ctorName fields)

checkTableCtor typeName _ =
	fail ("'" ++ show typeName ++ "' must have a normal or record constructor")

-- | Make sure the given declaration can be used, then construct a 'TableDec'.
checkTableDec :: Name -> Dec -> Q TableDec
checkTableDec _ (DataD ctx typeName typeVars kind ctors _) = do
	unless (null ctx) $
		fail ("'" ++ show typeName ++ "' must not have a context")

	unless (null typeVars) $
		fail ("'" ++ show typeName ++ "' must not use type variables")

	unless (length ctors == 1) $
		fail ("'" ++ show typeName ++ "' must have 1 constructor")

	unless (kind == Just StarT || kind == Nothing) $
		fail ("'" ++ show typeName ++ "' must have kind *")

	checkTableCtor typeName (head ctors)

checkTableDec typeName _ =
	fail ("'" ++ show typeName ++ "' must declare a data type")

-- | Reify table information in order to fetch the table declaration.
checkTableName :: Name -> Q TableDec
checkTableName typeName = do
	info <- reify typeName
	case info of
		TyConI dec -> checkTableDec typeName dec
		_          -> fail ("'" ++ show typeName ++ "' is not a type constructor")

-- | Option for creating a table.
data TableOptions = TableOptions {
	tableOptIdentName          :: B.ByteString,
	tableOptTransformName      :: Name -> B.ByteString,
	tableOptTransformFieldName :: Name -> B.ByteString
}

-- | Default table options.
defaultTableOptions :: TableOptions
defaultTableOptions =
	TableOptions {
		tableOptIdentName          = "$id",
		tableOptTransformName      = B.toByteString . B.fromString . nameBase,
		tableOptTransformFieldName = B.toByteString . B.fromString . nameBase
	}

-- |
data InterimTableConstraint
	= Unique [Name]
	| Check B.ByteString

-- | Implement 'Table' for a type.
implementTable :: TableDec -> [InterimTableConstraint] -> TableOptions -> Q [Dec]
implementTable (TableDec typeName ctor fields) constraints TableOptions {..} =
	genVarNames >>= \ boundNames ->
		[d|
			instance Table $(conT typeName) where
				tableInfo _ =
					TableInformation
						$(liftByteString (tableOptTransformName typeName))
						$(liftByteString tableOptIdentName)
						$(listE (map genColumn fields))
						$(listE (map genConstraint constraints))

				unpackRow $(destructPattern boundNames) =
					$(ListE <$> mapM genPackColumn boundNames)
		|]
	where
		genColumn (TableField name typ) =
			[e| ($(liftByteString (tableOptTransformFieldName name)),
			     columnInfo (Proxy :: Proxy $(pure typ))) |]

		genVarNames =
			mapM (\ (TableField fieldName _) -> newName (nameBase fieldName)) fields

		destructPattern names =
			pure (ConP ctor (map VarP names))

		genPackColumn name =
			[e| pack $(varE name) |]

		genConstraint constraint =
			case constraint of
				Unique names ->
					[e| TableUnique $(listE (map (liftByteString . tableOptTransformFieldName) names)) |]

				Check statement ->
					[e| TableCheck $(liftByteString statement) |]

-- | Implement 'QueryEntity' for a type.
implementQueryEntity :: TableDec -> Q [Dec]
implementQueryEntity (TableDec typeName ctor fields) =
	genVarNames >>= \ boundNames ->
		[d|
			instance QueryEntity $(conT typeName) where
				insertEntity row =
					insertEntity (unpackRow row)
		|]
	where
		genVarNames =
			mapM (\ (TableField fieldName _) -> newName (nameBase fieldName)) fields

-- | Implement 'Result' for a type.
implementResult :: TableDec -> Q [Dec]
implementResult (TableDec typeName ctor fields) =
	genVarNames >>= \ boundNames ->
		[d|
			instance Result $(conT typeName) where
				queryResultProcessor =
					$(pure (functionBody boundNames))
		|]
	where
		genVarNames =
			mapM (\ (TableField fieldName _) -> newName (nameBase fieldName)) fields

		doBinding name =
			BindS (VarP name) (VarE 'unpackColumn)

		constructValue names =
			AppE (VarE 'pure)
			     (foldl (\ a b -> AppE a (VarE b)) (ConE ctor) names)

		functionBody names =
			DoE (map doBinding names ++ [NoBindS (constructValue names)])

-- | Implement 'Table', 'Result' and 'QueryEntity' for a type.
makeTable :: Name -> [InterimTableConstraint] -> TableOptions -> Q [Dec]
makeTable typeName constraints options = do
	dec <- checkTableName typeName
	concat <$> sequence [implementTable dec constraints options,
	                     implementResult dec,
	                     implementQueryEntity dec]

-- | Create a table using the given 'TableInformation'.
createTable :: TableInformation -> Query ()
createTable TableInformation {..} =
	[pgsq| CREATE TABLE ${insertName tableName} ($columns) |]
	where
		genColumn name ColumnInformation {..} = do
			insertName name
			insertCode " "
			insertCode columnTypeName

			unless columnAllowNull $
				insertCode " NOT NULL"

			case columnCheck of
				Just genStmt -> do
					insertCode " CHECK ("
					insertCode (genStmt (buildQuery (insertName name)))
					insertCode ")"

				Nothing ->
					pure ()

		identColumn = do
			insertName tableIdentColumn
			insertCode " SERIAL PRIMARY KEY NOT NULL"

		genConstraint constraint =
			case constraint of
				TableUnique names ->
					[pgsq| UNIQUE (${map insertName names}) |]

				TableCheck code ->
					[pgsq| CHECK (${insertCode code}) |]

		columns =
			identColumn : map (uncurry genColumn) tableColumns ++ map genConstraint tableConstraints

-- | Create the given table.
createTable_ :: (Table a) => proxy a -> Query ()
createTable_ proxy =
	createTable (tableInfo proxy)