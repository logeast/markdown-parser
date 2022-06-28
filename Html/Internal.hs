module Html.Internal where

-- A complete Html document.
newtype Html = Html String

-- A type for html structures, such as head, h1, p, etc.
newtype Structure = Structure String

-- Title is just a type name alias.
type Title = String

-- Combine html tags.
html_ :: Title -> Structure -> Html
html_ title content =
  Html
    ( el
        "html"
        ( el "head" (el "title" (escape title))
            <> el "body" (getStructureString content)
        )
    )

-- Because,
-- Structure :: String -> Structure
-- el "p" :: String -> String
-- So,
-- a ~ String
-- b ~ String
-- c ~ Structure
-- Because,
-- (.) :: (b -> c) -> (a -> b) -> (a -> c)
-- So,
-- (.) :: (String -> Structure) -> (String -> String) -> (String -> Structure)
-- So,
-- Structure . el "p" :: String -> Structure
p_ :: String -> Structure
p_ = Structure . el "p" . escape

h1_ :: String -> Structure
h1_ = Structure . el "h1" . escape

ul_ :: [Structure] -> Structure
ul_ = Structure . el "ul" . concat . map (el "li" . getStructureString)

ol_ :: [Structure] -> Structure
ol_ = Structure . el "ol" . concat . map (el "li" . getStructureString)

code_ :: String -> Structure
code_ = Structure . el "pre" . escape

-- A function that take a tag and content, and wraps the content with the tag.
el :: String -> String -> String
el tag content =
  "<" <> tag <> ">" <> content <> "</" <> tag <> ">"

-- Replace <> operator. Take two Structures and return a third Structure.
append_ :: Structure -> Structure -> Structure
append_ (Structure a) (Structure b) = Structure (a <> b)

-- Extract the String out of type Structure.
getStructureString :: Structure -> String
getStructureString content =
  case content of
    Structure str -> str

-- Converting back Html to String. So we can display it in our browser.
render :: Html -> String
render html =
  case html of
    Html str -> str

-- Escaping characters
escape :: String -> String
escape =
  let escapeChar c =
        case c of
          '<' -> "&lt;"
          '>' -> "&gt;"
          '&' -> "&amp;"
          '"' -> "&quot;"
          '\'' -> "&#39;"
          _ -> [c]
   in concat . map escapeChar
