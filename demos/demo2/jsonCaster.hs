{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module JSONCaster where

import Demo2Shared as D2
import GHC.Generics
import Data.Aeson
import Data.Map
import Data.Text
import Data.ByteString.Lazy
--import Data.ByteString.Internal as IB
data DesiredEvidenceWrapper = DEW {desiredEvidence :: DesiredEvidence} deriving (Generic,Show)
data EvidenceDescriptorWrapper = EDW {evidenceDescriptor :: EvidenceDescriptor} deriving (Generic, Show)
data EvidencePieceWrapper = EPW {evidencePiece :: EvidencePiece2} deriving (Generic, Show)

data EvidencePiece2 = M02 M02Rep
                      | M12 M12Rep
                      | M22 M22Rep deriving (Generic, Show)
                        
type M02Rep = String --Data.ByteString.Lazy.ByteString
type M12Rep = String --Data.ByteString.Lazy.ByteString
type M22Rep = String --Data.ByteString.Lazy.ByteString
instance FromJSON DesiredEvidenceWrapper
instance ToJSON DesiredEvidenceWrapper
instance ToJSON EvidenceDescriptor
instance FromJSON EvidenceDescriptor
instance ToJSON EvidencePieceWrapper
instance FromJSON EvidencePieceWrapper
instance ToJSON EvidencePiece2
instance FromJSON EvidencePiece2

--test = DEW [D0, D1,D2,D2]


stripED :: EvidenceDescriptorWrapper -> EvidenceDescriptor
stripED x = evidenceDescriptor x
