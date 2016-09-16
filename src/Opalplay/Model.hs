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

data PassengerTag

data Passenger id name cat = Passenger
  { passengerId        :: id
  , passengerName      :: name
  , passengerCreatedAt :: cat
  }

type PassengerColumns = Passenger
  (Column (PGId PassengerTag))
  (Column PGText)
  (Column PGTimestamptz)

type PassengerColumnsWrite = Passenger
  (Maybe (Column (PGId PassengerTag)))
  (Column PGText)
  (Maybe (Column PGTimestamptz))

type PassengerData =
  Passenger (Id PassengerTag) Text UTCTime

makeAdaptorAndInstance "pPassenger" ''Passenger

passengersTable :: Table PassengerColumnsWrite PassengerColumns
passengersTable = Table "passengers" $ pPassenger $ Passenger
  { passengerId = optional "id"
  , passengerName = required "name"
  , passengerCreatedAt = optional "created_at"
  }

data ProductTag

data Product id name oid typ pid cat = Product
  { productId          :: id
  , productName        :: name
  , productOrderId     :: oid
  , productProductType :: typ
  , productProductId   :: pid
  , productCreatedAt   :: cat
  }

type ProductColumns =
  Product (Column (PGId ProductTag)) (Column PGText) (Column (PGId OrderTag)) (Column PGText) (Column PGUuid) (Column PGTimestamptz)

type ProductData =
  Product (Id ProductTag) Text (Id OrderTag) Text UUID UTCTime

makeAdaptorAndInstance "pProduct" ''Product

productsTable :: Table
  (Product (Maybe (Column (PGId ProductTag))) (Column PGText) (Column (PGId OrderTag)) (Column PGText) (Column PGUuid) (Maybe (Column PGTimestamptz)))
  ProductColumns
productsTable = Table "products" $ pProduct $ Product
  { productId          = optional "id"
  , productName        = required "name"
  , productOrderId     = required "order_id"
  , productProductType = required "product_type"
  , productProductId   = required "product_id"
  , productCreatedAt   = optional "created_at"
  }

data AirTicketProduct id name cat = AirTicketProduct
  { airTicketProductId        :: id
  , airTicketProductName      :: name
  , airTicketProductCreatedAt :: cat
  }

type AirTicketProductColumns = AirTicketProduct
  (Column PGUuid)
  (Column PGText)
  (Column PGTimestamptz)

type AirTicketProductColumnsWrite = AirTicketProduct
  (Maybe (Column PGUuid))
  (Column PGText)
  (Maybe (Column PGTimestamptz))

makeAdaptorAndInstance "pAirTicketProduct" ''AirTicketProduct

airTicketProductsTable :: Table AirTicketProductColumnsWrite AirTicketProductColumns
airTicketProductsTable = Table "air_tickets" $ pAirTicketProduct $ AirTicketProduct
  { airTicketProductId        = optional "id"
  , airTicketProductName      = required "name"
  , airTicketProductCreatedAt = optional "created_at"
  }

data SurchageProduct id name cat = SurchageProduct
  { surchageProductId        :: id
  , surchageProductName      :: name
  , surchageProductCreatedAt :: cat
  }

type SurchageProductColumns = SurchageProduct
  (Column PGUuid)
  (Column PGText)
  (Column PGTimestamptz)

type SurchageProductColumnsWrite = SurchageProduct
  (Maybe (Column PGUuid))
  (Column PGText)
  (Maybe (Column PGTimestamptz))

makeAdaptorAndInstance "pSurchageProduct" ''SurchageProduct

surchageProductsTable :: Table SurchageProductColumnsWrite SurchageProductColumns
surchageProductsTable = Table "air_tickets" $ pSurchageProduct $ SurchageProduct
  { surchageProductId        = optional "id"
  , surchageProductName      = required "name"
  , surchageProductCreatedAt = optional "created_at"
  }
