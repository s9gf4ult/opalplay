module Opalplay.Model where

import Data.Profunctor.Product.TH
import Data.Proxy
import Data.Text (Text)
import Data.Time
import Data.UUID
import Opaleye.Column
import Opaleye.PGTypes
import Opaleye.Table

import qualified Data.Text as T

newtype PGId a = PGId
  { unPGOrderId :: PGUuid
  }

instance IsSqlType (PGId a) where
  showPGType _ = showPGType (Proxy :: Proxy PGUuid)

newtype Id a = Id
  { unId :: UUID
  } deriving (Ord, Eq, Show)

data OrderTag

data Order id name cr = Order
  { orderId        :: id
  , orderName      :: name
  , orderCreatedAt :: cr
  }

type OrderColumns =
  Order (Column (PGId OrderTag)) (Column PGText) (Column PGTimestamptz)
type OrderData =
  Order (Id OrderTag) Text UTCTime

makeAdaptorAndInstance "pOrder" ''Order

ordersTable :: Table
  ( Order (Maybe (Column (PGId OrderTag))) (Column PGText) (Maybe (Column PGTimestamptz)) )
  OrderColumns
ordersTable = Table "orders" $ pOrder $ Order
  { orderId        = optional "id"
  , orderName      = required "name"
  , orderCreatedAt = optional "created_at"
  }

data Product id name oid typ pid cat = Product
  { productId          :: id
  , productName        :: name
  , productOrderId     :: oid
  , productProductType :: typ
  , productProductId   :: pid
  , productCreatedAt   :: cat
  }

type ProductColumns =
  Product (Column PGUuid) (Column PGText) (Column (PGId OrderTag)) (Column PGText) (Column PGUuid) (Column PGTimestamptz)

type ProductData =
  Product UUID Text (Id OrderTag) Text UUID UTCTime

makeAdaptorAndInstance "pProduct" ''Product

productsTable :: Table
  (Product (Maybe (Column PGUuid)) (Column PGText) (Column (PGId OrderTag)) (Column PGText) (Column PGUuid) (Maybe (Column PGTimestamptz)))
  ProductColumns
productsTable = Table "products" $ pProduct $ Product
  { productId          = optional "id"
  , productName        = required "name"
  , productOrderId     = required "order_id"
  , productProductType = required "product_type"
  , productProductId   = required "product_id"
  , productCreatedAt   = optional "created_at"
  }
