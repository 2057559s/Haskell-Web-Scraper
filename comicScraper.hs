{-# LANGUAGE OverloadedStrings #-}
import Text.LaTeX
import Data.List (isInfixOf)
import Data.List.Split
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
    let d2 =  map fromJust (fromJust details)
    let d3 = map (map tostring) d2
    let d4 = (filter (not . null) d3)
    --sequence_ details
    print d4
    text <- execLaTeXT doc
    renderFile "test.tex" text


flattenArray :: [[(String)]] -> [(String)]
flattenArray [[(a)]] = [(a)]

--extractDetails :: [[String]] -> [String]
--extractDetails (x:xs) = scrapeURL x scrapeDetails : getDetails xs
tostring :: (String, String) -> String
tostring (name, phone) = "Name: " ++ name ++ " Phone: " ++ phone

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
{-
scrapeDetails :: Scraper String [String]
scrapeDetails =  
    chroots "h1" ("div" @: ["id" @= "sp_contactInfo"])
-}


scrapeLinks :: Scraper String [String]
scrapeLinks =  
    chroots "a" $ do
        altText <- attr "href" anySelector -- altText is the staff name, gets the name if the url is after schools/comutin
        guard (staffUrl `isInfixOf` altText)
        let url = mainUrl ++ altText
        return url

{-}
fileWrite >> "details.txt" details

\input{"details.txt"}

-}

--Just [("Dr Mary Ellen Foster","\ntelephone: 0141 330 4742\nemail: MaryEllen.Foster@glasgow.ac.uk\n")]
{-}
Just [["\n<strong>email</strong>: <a href=\"mailto:Abeer.Ali@glasgow.ac.uk\">Abeer.Ali@glasgow.ac.uk</a>"]]
Just [["\n<strong>telephone</strong>: 01413305457<br></br>\n<strong>email</strong>: <a href=\"mailto:Oana.Andrei@glasgow.ac.uk\">Oana.Andrei@glasgow.ac.uk</a>"]]

-}
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
    author "Nicholas Saunderson"
    title "Function Programming 4"

theBody :: Monad m => LaTeXT m ()
theBody = do
    maketitle
    center $ tabular Nothing [RightColumn,VerticalLine,LeftColumn] $ do
        textbf "Name" & textbf "Job"
        lnbk 
        hline
        do
            textit "Nick"
            &
            textit "Foo"
{-
scrapeStaffURL :: Scraper String [[String]]
scrapeStaffURL =
    chroots ("a") scrapeTitle


scrapeTitle :: Scraper String [String]
scrapeTitle = do
    staffLink <- attr "href" anySelector
    guard ("schools/computing/staff/" `isInfixOf` staffLink)
    let url = "www.gla.ac.uk" ++ staffLink
    print url

    -}







{-}
import Text.HTML.Scalpel
import Control.Applicative
import Control.Monad
import Data.List (isInfixOf)
import Data.Char  

main :: IO ()
main = do
    links <- getLinks "http://www.gla.ac.uk//schools/computing/staff/"
    details <- return (fmap (mapM getDetails) links)
    sequence_ details
    print "done"

getLinks :: String -> IO (Maybe [String])
getLinks a = scrapeURL a scrapeLinks

getDetails :: String -> IO (Maybe [String])
getDetails a = scrapeURL a scrapeDetails

scrapeDetails :: Scraper String [String]
scrapeDetails =  
    chroots "h1" $ do
        name <- text anySelector
        let n = "name: " ++ name
        print 
        return n


scrapeLinks :: Scraper String [String]
scrapeLinks =  
    chroots "a" $ do
        altText <- attr "href" anySelector
        guard ("schools/computing/staff" `isInfixOf` altText)
        let url = "http://www.gla.ac.uk/" ++ altText
        return url

        --}