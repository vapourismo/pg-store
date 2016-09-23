{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

-- |
-- Module:     Database.PostgreSQL.Store.Table.Class
-- Copyright:  (c) Ole Krüger 2016
-- License:    BSD3
-- Maintainer: Ole Krüger <ole@vprsm.de>
module Database.PostgreSQL.Store.Table.TH (
	checkTableName,
	implementTable
) where

import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax

import           Control.Monad

import           Data.Maybe

import qualified Data.ByteString                    as B
import qualified Blaze.ByteString.Builder           as B
import qualified Blaze.ByteString.Builder.Char.Utf8 as B

import           Database.PostgreSQL.Store.Table.Class
import           Database.PostgreSQL.Store.Columns

-- | Table field declaration
data TableField = TableField Name Type

-- | Table type declaration
data TableDec = TableDec Name Name [TableField]

-- | Retrieve the table's type name.
tableTypeName :: TableDec -> Name
tableTypeName (TableDec name _ _ ) = name

-- | Check that each field's type has an implementation of 'Column'.
checkRecordFields :: [VarBangType] -> Q [TableField]
checkRecordFields fields =
	forM fields $ \ (name, _, typ) -> do
		ii <- isInstance ''Column [typ]
		unless ii $
			reportWarning ("Type of field '"
			               ++ show name
			               ++ "' ('"
			               ++ show typ
			               ++ "') type does not implement '"
			               ++ show ''Column
			               ++ "'")

		pure (TableField name typ)

-- | Check that each constructor parameter type implements 'Column'.
checkNormalFields :: [BangType] -> Q [TableField]
checkNormalFields fields = do
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
	when (length ctx > 0)
	     (fail ("'" ++ show typeName ++ "' must not have a context"))

	when (length typeVars > 0)
	     (fail ("'" ++ show typeName ++ "' must not use type variables"))

	when (length ctors /= 1)
	     (fail ("'" ++ show typeName ++ "' must have 1 constructor"))

	when (kind /= Nothing && kind /= Just StarT)
	     (fail ("'" ++ show typeName ++ "' must have kind *"))

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

-- | Generate the table information using the table declaration.
toTableInfo :: TableDec -> Maybe B.ByteString -> TableInformation
toTableInfo (TableDec typeName _ fields) overrideName =
	TableInformation name "$id" (map buildField fields)
	where
		name =
			fromMaybe (B.toByteString (B.fromString (nameBase typeName))) overrideName

		buildField (TableField fieldName _) =
			B.toByteString (B.fromString (nameBase (fieldName)))

-- | Implement 'Table' for a type.
implementTable :: TableDec -> Maybe B.ByteString -> Q [Dec]
implementTable dec overrideName =
	[d|
		instance Table $(conT (tableTypeName dec)) where
			tableInfo _ = $(lift (toTableInfo dec overrideName))
	|]
