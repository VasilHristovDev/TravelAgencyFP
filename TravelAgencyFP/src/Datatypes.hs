module Datatypes
where 

newtype Agency = Agency [Customer]
            deriving (Show, Eq)

data Customer = Customer {name :: Name, trips :: [Trip], points :: Int}
              deriving (Show, Eq)

data Trip = Trip {destination :: Destination, price :: Float, status :: Status, activities :: [Activity], policy :: CancelPolicy}
          deriving (Show)

data Status = Upcoming | Canceled | InProgress | Finished
            deriving (Show, Eq)

data Activity = Activity String Int
                deriving (Show, Eq)

data Tresholds = Thresholds {moneyTreshold :: Float, durationTreshold :: Int, loyaltyPointsTreshold :: Int}
                deriving (Show, Eq)

data Refund = Nothing | Just Float
                deriving (Show, Eq)

type Name = String
type Destination = String

data CancelPolicy = Flexible | Moderate | Strict
                deriving (Show, Eq)

instance Eq Trip where
    (==) (Trip destination1 price1 _ activities1 policy1) (Trip destination2 price2 _ activities2 policy2) =
        destination1 == destination2 && price1 == price2 && activities1 == activities2 && policy1 == policy2


