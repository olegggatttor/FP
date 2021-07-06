module FirstBlock.Task1
  ( Day(..)
  , nextDay
  , afterDays
  , isWeekend
  , daysToParty
  ) where

-- | Days type.
data Day
  = Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  | Sunday
  deriving Show

instance Eq Day where
  (==) a b = show a == show b

instance Enum Day where
  toEnum 1     = Monday
  toEnum 2     = Tuesday
  toEnum 3     = Wednesday
  toEnum 4     = Thursday
  toEnum 5     = Friday
  toEnum 6     = Saturday
  toEnum 7     = Sunday
  toEnum other = toEnum $ mod other 7

  fromEnum Monday    = 1
  fromEnum Tuesday   = 2
  fromEnum Wednesday = 3
  fromEnum Thursday  = 4
  fromEnum Friday    = 5
  fromEnum Saturday  = 6
  fromEnum Sunday    = 7

-- | Returns next day after given one.
nextDay
  :: Day -- ^ given Day
  -> Day -- ^ next Day after given one
nextDay = succ

-- | Returns the day of the week after the specified number of days after the passed.
afterDays
  :: Day -- ^ given Day
  -> Int -- ^ number of days after
  -> Day -- ^ required Day
afterDays day 0 = day
afterDays day after
  | after > 0   = afterDays (nextDay day) (mod (after-1) 7)
  | after == 0  = day
  | otherwise   = afterDays day (mod after 7)

-- | Returns True if Day is weekend and False otherwise.
isWeekend
  :: Day  -- ^ given Day
  -> Bool -- ^ True if Day is weekend and False otherwise
isWeekend Saturday = True
isWeekend Sunday   = True
isWeekend _        = False

-- | displays the number of days remaining until Friday
daysToParty
  :: Day -- ^ given Day
  -> Int -- ^ amount of days until Friday
daysToParty = countDays 0 where
                            countDays :: Int -> Day -> Int
                            countDays days Friday = days
                            countDays days today  = countDays (days + 1) (nextDay today)
