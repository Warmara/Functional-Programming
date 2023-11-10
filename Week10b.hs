
-- :load 
-- thank yourself later

data PhoneType = WorkLandline | PrivateMobile | WorkMobile | Other deriving (Show, Eq, Read)

newtype CountryCode = CountryCode {getCountryCode :: Integer} deriving (Eq, Read) 

instance Show CountryCode where
 show (CountryCode x) = "+" ++ show x

newtype PhoneNo = PhoneNo {getPhoneNo :: Integer} deriving (Eq, Read)

instance Show PhoneNo where
 show (PhoneNo c) = show c

toCountryCode :: Integer -> CountryCode
toCountryCode num
 |num < 0 = error "Negative country code"
 |otherwise = CountryCode num

toPhoneNo :: Integer -> PhoneNo
toPhoneNo num
 |num < 0 = error "Negative phone number"
 |otherwise = PhoneNo num
 
data Phone = Phone {
phoneType :: Maybe PhoneType,
countryCode :: Maybe CountryCode,
phoneNo :: PhoneNo
} deriving (Eq)

instance Show Phone where
 show (Phone (Nothing) (Nothing) pn) = show pn
 show (Phone (Nothing) (Just cc) pn) = show cc ++ " " ++ show pn
 show (Phone (Just pt) (Nothing) pn) = show pn ++ " (" ++ show pt ++ ")"
 show (Phone (Just pt) (Just cc) pn) = show cc ++ " " ++ show pn ++ " (" ++ show pt ++ ")"


readPhone :: String -> String -> String -> [Integer] -> Phone
readPhone _ _ "" _ = error "Empty phone number"
readPhone "" "" phonenostr ccodelist = Phone (Nothing) (Nothing) (readPhoneNo phonenostr)
readPhone "" countrycodestr phonenostr ccodelist = Phone (Nothing) (readCountryCode countrycodestr ccodelist) (readPhoneNo phonenostr)
readPhone phonetypestr "" phonenostr ccodelist = Phone (readPhoneType  phonetypestr) (Nothing) (readPhoneNo phonenostr)
readPhone phonetypestr countrycodestr phonenostr ccodelist = Phone (readPhoneType  phonetypestr) (readCountryCode countrycodestr ccodelist) (readPhoneNo phonenostr)

-- PhoneType
readPhoneType  :: String -> Maybe PhoneType
readPhoneType  str
 |str == "WorkLandline" = (Just WorkLandline)
 |str == "PrivateMobile" = (Just PrivateMobile)
 |str == "WorkMobile" = (Just WorkMobile)
 |str == "Other" = (Just Other)
 |str == "" = Nothing
 |otherwise = error "Incorrect phone type"


-- CountryCode
readCountryCode :: String -> [Integer] -> Maybe CountryCode
readCountryCode "" _ = Nothing
readCountryCode str ccodelist = Just (toCountryCode $ validity (sToInt (crop str)) ccodelist)

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
readPhoneNo :: String -> PhoneNo
readPhoneNo (s:str)
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

cc :: Maybe CountryCode
cc = readCountryCode "420" [420]

pn :: PhoneNo
pn = readPhoneNo "123456789"