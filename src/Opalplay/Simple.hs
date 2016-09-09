module Opalplay.Simple where

import Control.Arrow
import Opaleye
import Opalplay.Model


qprods :: QueryArr (Column PGText) ProductColumns
qprods = proc pname -> do
  ord <- queryTable ordersTable -< ()
  prod <- queryTable productsTable -< ()
  restrict -< (orderId ord) .== (productOrderId prod) .&& (orderName ord) .== pname
  returnA -< prod
