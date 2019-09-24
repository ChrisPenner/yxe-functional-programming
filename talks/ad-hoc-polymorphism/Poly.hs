module Poly where

data ContactInfo
    = Email String
    | Phone String
    | Address String
    deriving (Show, Eq)

data User =
    User { contactInfo :: ContactInfo
         } deriving (Show, Eq)

type Total = Int

checkout :: Total -> User -> IO Bool
checkout total user = do
    successfulCharge <- charge total user
    if successfulCharge
       then contact (contactInfo user) >> return True
       else return False


contact :: ContactInfo -> IO ()
contact (Email s) = sendEmail s
contact (Phone phone) = sendText phone
contact (Address addr) = return ()

sendEmail = undefined
sendText = undefined
charge _ _ = return True
