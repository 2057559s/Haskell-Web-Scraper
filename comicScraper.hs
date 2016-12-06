{-# LANGUAGE OverloadedStrings #-}
import Text.LaTeX
import Data.List (isInfixOf)
import Data.List.Split
import Data.List (intercalate)
import Data.Char
import Text.HTML.Scalpel
import Data.Maybe
import Control.Monad
import Control.Applicative

main :: IO()
main = do
    -- Scrapes the main url link and stores within links
    links <- getLinks (mainUrl++staffUrl)
    details <- sequence $ (fmap (mapM getDetails) links)
    let filter1 =  map fromJust (fromJust details)
    let filter2 = map (map tostring) filter1
    let filter3 = (filter (not . null) filter2)
    --sequence_ details
    let flatten = intercalate []
    let flat = flatten $ flatten [filter3]
    writeFile "output.txt" $ unlines flat
    text <- execLaTeXT doc
    renderFile "test.tex" text


tostring :: (String, String) -> String
tostring (name, phone) = name ++ ", Phone: " ++ phone

mainUrl :: String
mainUrl = "http://www.gla.ac.uk/"

staffUrl :: String
staffUrl = "schools/computing/staff"

--Function for scraping the main link
getLinks :: String -> IO (Maybe [String])
getLinks a = scrapeURL a scrapeLinks

-- Function for scraping each individual staff link
getDetails :: String -> IO (Maybe [(String, String)])
getDetails a = scrapeURL a scrapeDetails


scrapeDetails :: Scraper String [(String, String)]
scrapeDetails =  
    chroots ("div" @: ["id" @= "mainContent"]) getAttributes
        

getAttributes :: Scraper String (String, String)
getAttributes = do
    name <- text "h1"
    telephone <- text ("div" @: ["id" @= "sp_contactInfo"]) 
    return (name, getPhoneNumber telephone)


scrapeLinks :: Scraper String [String]
scrapeLinks =  
    chroots "a" $ do
        altText <- attr "href" anySelector -- altText is the staff name, gets the name if the url is after schools/comutin
        guard (staffUrl `isInfixOf` altText)
        let url = mainUrl ++ altText
        return url


getPhoneNumber :: String -> String 
getPhoneNumber telephoneString = do
    if ("\ntelephone: " `isInfixOf` telephoneString )
        then (formatPhoneNumber telephoneString)
        else ("no phone")

formatPhoneNumber :: String -> String
formatPhoneNumber telephoneString = (splitOn "\n" ((splitOn ": " telephoneString)!!1))!!0




doc :: Monad m => LaTeXT m ()
doc = do 
    thePreamble
    document theBody

thePreamble :: Monad m => LaTeXT m ()
thePreamble = do
    documentclass [] article 
    usepackage [] "verbatim"
    author "Nicholas Saunderson"
    title "Function Programming 4 - Phone Book"

theBody :: Monad m => LaTeXT m ()
theBody = do
    maketitle
    lnbk 
    hline
    raw "\\verbatiminput{output.txt}"


