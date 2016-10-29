-- |
-- Module:     Database.PostgreSQL.Store.Result.Entity
-- Copyright:  (c) Ole Krüger 2016
-- License:    BSD3
-- Maintainer: Ole Krüger <ole@vprsm.de>
module Database.PostgreSQL.Store.Result.Entity (
	-- * Result Entity
	ResultEntity (..)
) where

import           Control.Monad
import           Control.Applicative

import           Database.PostgreSQL.Store.Result.Parser


-- | An entity whose underlying information spans zero or more columns
class ResultEntity a where
	parseEntity :: RowParser a

instance (ResultEntity a, ResultEntity b) => ResultEntity (a, b) where
	parseEntity =
		liftA2 (,) parseEntity parseEntity

instance (ResultEntity a, ResultEntity b, ResultEntity c) => ResultEntity (a, b, c) where
	parseEntity =
		liftA3 (,,) parseEntity parseEntity parseEntity

instance (ResultEntity a, ResultEntity b, ResultEntity c, ResultEntity d)
         => ResultEntity (a, b, c, d) where
	parseEntity =
		liftM4 (,,,) parseEntity parseEntity parseEntity parseEntity

instance (ResultEntity a, ResultEntity b, ResultEntity c, ResultEntity d, ResultEntity e)
         => ResultEntity (a, b, c, d, e) where
	parseEntity =
		liftM5 (,,,,) parseEntity parseEntity parseEntity parseEntity parseEntity

instance (ResultEntity a, ResultEntity b, ResultEntity c, ResultEntity d, ResultEntity e,
          ResultEntity f)
         => ResultEntity (a, b, c, d, e, f) where
	parseEntity =
		(,,,,,) <$> parseEntity <*> parseEntity <*> parseEntity <*> parseEntity <*> parseEntity
		        <*> parseEntity

instance (ResultEntity a, ResultEntity b, ResultEntity c, ResultEntity d, ResultEntity e,
          ResultEntity f, ResultEntity g)
         => ResultEntity (a, b, c, d, e, f, g) where
	parseEntity =
		(,,,,,,) <$> parseEntity <*> parseEntity <*> parseEntity <*> parseEntity <*> parseEntity
		         <*> parseEntity <*> parseEntity

instance (ResultEntity a, ResultEntity b, ResultEntity c, ResultEntity d, ResultEntity e,
          ResultEntity f, ResultEntity g, ResultEntity h)
         => ResultEntity (a, b, c, d, e, f, g, h) where
	parseEntity =
		(,,,,,,,) <$> parseEntity <*> parseEntity <*> parseEntity <*> parseEntity <*> parseEntity
		          <*> parseEntity <*> parseEntity <*> parseEntity
