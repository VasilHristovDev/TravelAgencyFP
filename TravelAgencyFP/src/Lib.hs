module Lib
   where
import Datatypes

agency :: Agency
agency = Agency [customer1, customer2, customer3]
    where customer1 = Customer "Ivan" [malaga, barcelona, sofia] 150
          customer2 = Customer "Nikolai" [spain, napoli, monteChristo] 205
          customer3 = Customer "Kiril" [malaga, sofia, moscow] 400
          malaga = Trip "Malaga" 1500.8 InProgress [skiing, sightSeeing, swimming] Flexible
          barcelona = Trip "Barcelona" 3000 Finished [beach, swimming, sightSeeing] Moderate
          sofia = Trip "Sofia" 1000.20 Canceled [sightSeeing] Strict
          spain = Trip "Spain" 1200.30 Finished [sightSeeing, swimming] Flexible
          napoli = Trip "Napoli" 1400.50 Finished [sightSeeing, clubbing] Moderate
          monteChristo = Trip "Monte Christo" 1250.15 InProgress [swimming, beach] Flexible
          moscow = Trip "Moscow" 2400.40 InProgress [sightSeeing, clubbing] Strict
          skiing = Activity "Ski practise" 120
          sightSeeing = Activity "Sightseeing the most famous places" 300
          swimming = Activity "Swimming" 90
          beach = Activity "Going sunbathing" 120
          clubbing = Activity "Going to the clubs" 200

thresholds :: Tresholds
thresholds = Thresholds 20 20 200

someFunc :: IO ()
someFunc = putStrLn "someFunc"

freeEligibleCustomers :: Agency -> Tresholds -> [Customer]
freeEligibleCustomers (Agency []) _ = []
freeEligibleCustomers (Agency (customer:customers)) tresholds = 
    if isEligibleCustomer customer tresholds
    then customer : freeEligibleCustomers (Agency customers) tresholds
    else freeEligibleCustomers (Agency customers) tresholds

isEligibleCustomer :: Customer -> Tresholds -> Bool
isEligibleCustomer (Customer _ trips points) (Thresholds money durations loyaltyPointsThreshold) = 
    points > loyaltyPointsThreshold 
    && hasEnoughDurations trips durations
    && hasSpentEnoughMoney trips money
    && hasNotCancelledAnyTrip trips

sumTripDurations :: [Trip] -> Int
sumTripDurations [] = 0
sumTripDurations ((Trip _ _ _ activities _):trips) = sumActivitiesDuration activities + sumTripDurations trips

sumActivitiesDuration :: [Activity] -> Int
sumActivitiesDuration [] = 0
sumActivitiesDuration ((Activity _ duration): activities) = duration + sumActivitiesDuration activities

hasEnoughDurations :: [Trip] -> Int -> Bool
hasEnoughDurations [] _ = False
hasEnoughDurations trips durationThreshold = sumTripDurations trips > durationThreshold

sumTripMoney :: [Trip] -> Float
sumTripMoney [] = 0
sumTripMoney ((Trip _ money _ _ _):trips) = money + sumTripMoney trips

hasSpentEnoughMoney :: [Trip] -> Float -> Bool
hasSpentEnoughMoney [] _ = False
hasSpentEnoughMoney trips moneyTreshold = sumTripMoney trips > moneyTreshold

hasNotCancelledAnyTrip :: [Trip] -> Bool
hasNotCancelledAnyTrip [] = True
hasNotCancelledAnyTrip ((Trip _ _ status _ _):trips) = status /= Canceled && hasNotCancelledAnyTrip trips

getRefund :: Agency -> Name -> Destination -> Refund
getRefund _ "" _ = Datatypes.Nothing
getRefund _ _ "" = Datatypes.Nothing
getRefund (Agency []) _ _ = Datatypes.Nothing
getRefund agency customerName destinationName = 
    calculateRefund agency customer trip 
    where customer = getCustomerByName agency customerName
          trip = getTripFromCustomer customer destinationName

getCustomerByName :: Agency -> Name -> Customer
getCustomerByName (Agency []) _ = error "No customer found"
getCustomerByName _ "" = error "No customer name given"
getCustomerByName (Agency (x@(Customer name _ _): customers)) customerName = 
    if name == customerName
    then x
    else getCustomerByName (Agency customers) customerName

getTripFromCustomer :: Customer -> Destination -> Trip
getTripFromCustomer _ "" = error "No destination speciffied"
getTripFromCustomer (Customer _ [] _) _ = error "No trip found"
getTripFromCustomer (Customer name (x@(Trip destination _ _ _ _):trips) points) destinationName = 
    if destination == destinationName
    then x
    else getTripFromCustomer (Customer name trips points) destinationName

calculateRefund :: Agency -> Customer -> Trip -> Refund
calculateRefund (Agency customers) y@(Customer _ trips _) x@(Trip _ price _ _ policy) = 
    Datatypes.Just refund
    where refund =  max (( k * c ) * ( price - fee )) 0.0
          k = fromIntegral $ countNotCanceled `div` countAll
          c = getC policy
          fee = fromIntegral quotient * canceledAvg
          quotient = countCanceledTrip `div` countAllInTrip
          countNotCanceled = getCountNotCanceled trips
          countAll = length trips
          countCanceledTrip = countCustomersWhoCanceledTrip customers x
          countAllInTrip = countCustomersInTrip customers x
          canceledAvg = averagePriceCanceledTrips $ removeTrip y x
          

getC :: CancelPolicy -> Float
getC cancelPolicy
     | cancelPolicy == Flexible = 0.7
     | cancelPolicy == Moderate = 0.4
     | cancelPolicy == Strict = 0.1
     | otherwise  = 0.0

getCountNotCanceled :: [Trip] -> Int
getCountNotCanceled [] = 0
getCountNotCanceled trips = length (filter (\(Trip _ _ status _ _) -> status /= Canceled) trips)

countCustomersWhoCanceledTrip :: [Customer] -> Trip -> Int
countCustomersWhoCanceledTrip [] _ = 0
countCustomersWhoCanceledTrip (customer:customers) trip = 
    if hasCancelledTrip customer trip
    then 1 + countCustomersWhoCanceledTrip customers trip
    else countCustomersWhoCanceledTrip customers trip


hasCancelledTrip :: Customer -> Trip -> Bool
hasCancelledTrip (Customer _ [] _) _ = False
hasCancelledTrip (Customer name (x@(Trip _ _ status _ _):trips) points ) trip =
    (x == trip && status == Canceled)
    || hasCancelledTrip (Customer name trips points) trip

countCustomersInTrip :: [Customer] -> Trip -> Int
countCustomersInTrip [] _ = 0
countCustomersInTrip (customer:customers) trip = 
    if hasTrip customer trip
    then 1 + countCustomersWhoCanceledTrip customers trip
    else countCustomersWhoCanceledTrip customers trip

hasTrip :: Customer -> Trip -> Bool
hasTrip (Customer _ [] _) _ = False
hasTrip x@(Customer _ (trip: _) _) tripToCheck = 
    (trip == tripToCheck)
    || hasTrip x tripToCheck

averagePriceCanceledTrips :: Customer  -> Float
averagePriceCanceledTrips (Customer _ [] _) = 0
averagePriceCanceledTrips (Customer name ((Trip _ price status _ _):trips) p) = 
    if status == Canceled
    then price + averagePriceCanceledTrips (Customer name trips p)
    else averagePriceCanceledTrips (Customer name trips p)

removeTrip :: Customer -> Trip -> Customer
removeTrip x@(Customer _ [] _) _ = x
removeTrip (Customer name (trip:trips) points) tripToCheck = 
    if trip == tripToCheck
    then Customer name trips points
    else removeTrip (Customer name trips points) tripToCheck