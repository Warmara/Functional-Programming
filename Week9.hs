
-- :load 
-- thank yourself later

data PhoneType = WorkLandline | PrivateMobile | WorkMobile | Other deriving (Show, Eq)

type CountryCode = Integer

type PhoneNo = Integer

data Phone = Phone {
 phoneType :: PhoneType,
 countryCode :: CountryCode,
 phoneNo :: PhoneNo
} deriving (Show, Eq)

makePhone :: PhoneType -> CountryCode  -> PhoneNo -> Phone
makePhone pType cCode pNo
 |cCode < 0 = error "Negative country code"
 |pNo < 0 = error "Negative phone number"
 |otherwise = Phone pType cCode pNo