module TagTea
	( Doctype(HTML5, Other)
	, Name()
	, Attributes(Attributes)
	, HTML(Tag, Text, NoContent)
	, Document(Document)
	) where

import Prelude

import Data.Maybe  (Maybe(Just))
import Data.StrMap (StrMap(), fold)

data Doctype = HTML5
			 | Other String

instance showDoctype :: Show Doctype where
	show HTML5     = "<!DOCTYPE html>"
	show (Other o) = "<!DOCTYPE " <> o <> ">"

type Name          = String
newtype Attributes = Attributes (StrMap (Array String))

foreign import join   :: Char -> Array String -> String
foreign import length :: forall a. Array a -> Int

instance showAttributes :: Show Attributes where
	show (Attributes a) =  fold folder "" a
		where
			folder attrs attr values
				| length values == 0 = attrs
				| otherwise          = attrs <> " "
					<> attr <> "=\"" <> join ' ' values <> "\""

data HTML = Tag Name Attributes HTML
		  | Text String
		  | NoContent

instance showHTML :: Show HTML where
	show NoContent           = ""
	show (Text t)            = t
	show (Tag t a NoContent) = "<" <> t <> show a <> "></" <> t <> ">"
	show (Tag t a h)         = "<" <> t <> show a <> ">" <> show h
		<> "</" <> t <> ">"

data Document = Document
	{ doctype :: Doctype
	, head    :: Maybe HTML
	, body    :: HTML
	, invalid :: Boolean
	}

instance showDocument :: Show Document where
	show (Document { doctype, head, body, invalid }) = show doctype
		<> "<html>"
		<> "<head><meta name=\"generator\" content=\"purescript-tagtea=v1.0.0-dev\">"
		<> show' head <> "</head>"
		<> "<body>" <> show  body <> "</body>"
		<> "</html>"
		where
			show' (Just a) = show a
			show' _        = ""
