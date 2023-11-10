
-- :load 
-- thank yourself later

data PhoneType = WorkLandline | PrivateMobile | WorkMobile | Other deriving (Show, Eq, Read)

data CountryCode = CountryCode Integer deriving (Show, Eq, Read) 

data PhoneNo = PhoneNo Integer deriving (Show, Eq, Read)

toCountryCode :: Integer -> CountryCode
toCountryCode num
 |num < 0 = error "Negative country code"
 |otherwise = CountryCode num

toPhoneNo :: Integer -> PhoneNo
toPhoneNo num
 |num < 0 = error "Negative phone number"
 |otherwise = PhoneNo num
 
data Phone = Phone {
phoneType :: PhoneType,
countryCode :: CountryCode,
phoneNo :: PhoneNo
} deriving (Show, Eq)


readPhone :: String -> String -> String -> [Integer] -> Phone
readPhone "" _ _ _ = error "Missing phone type"
readPhone _ "" _ _ = error "Empty country code"
readPhone _ _ "" _ = error "Empty phone number"
readPhone phonetypestr countrycodestr phonenostr ccodelist = Phone (strToPhoneType phonetypestr) (strToCountryCode countrycodestr ccodelist) (strToPhoneNo phonenostr)

-- PhoneType
strToPhoneType :: String -> PhoneType
strToPhoneType str
 |str == "WorkLandline" = WorkLandline
 |str == "PrivateMobile" = PrivateMobile
 |str == "WorkMobile" = WorkMobile
 |str == "Other" = Other
 |otherwise = error "Incorrect phone type"


-- CountryCode
strToCountryCode :: String -> [Integer] -> CountryCode
strToCountryCode (str) ccodelist = toCountryCode $ validity (sToInt str) ccodelist 

validity :: Integer -> [Integer] -> Integer
validity code ccodelist
 |elem code ccodelist = code
 |otherwise = error "Unknown country code"

sToInt :: String -> Integer
sToInt str
 |onlyDigits str = read str :: Integer
 |otherwise = error "Incorrect country code"

crop :: String -> String
crop (sa:sb:str)
 |sa == '+' = (sb:str)
 |sa == '0' && sb == '0' = str
 |otherwise = (sa:sb:str)


-- PhoneNo
strToPhoneNo :: String -> PhoneNo
strToPhoneNo (s:str)
 |onlyDigits (s:str) = toPhoneNo (read (s:str) :: Integer)
 |s == '-' && onlyDigits str = error "Negative phone number"
 |otherwise = error "Incorrect phone number"



------------
isDigit :: Char -> Bool
isDigit '1' = True
isDigit '2' = True
isDigit '3' = True
isDigit '4' = True
isDigit '5' = True
isDigit '6' = True
isDigit '7' = True
isDigit '8' = True
isDigit '9' = True
isDigit '0' = True
isDigit _ = False

onlyDigits :: String -> Bool
onlyDigits [] = False
onlyDigits lst = subDigits lst True
 where
 subDigits [] result = result
 subDigits _ False = False
 subDigits (fst:rst) True
  |(isDigit fst) = subDigits (rst) True
  |otherwise = False