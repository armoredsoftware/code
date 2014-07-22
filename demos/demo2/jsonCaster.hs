{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module JSONCaster where

import Demo2Shared as D2
import GHC.Generics
import Data.Aeson
import Data.Map
import Data.Text
data DesiredEvidenceWrapper = DEW {desiredEvidence :: DesiredEvidence} deriving (Generic,Show)
data EvidenceDescriptorWrapper = EDW {evidenceDescriptor :: EvidenceDescriptor} deriving (Generic, Show)
data EvidencePieceWrapper = EPW {evidencePiece :: EvidencePiece} deriving (Generic, Show)


instance FromJSON DesiredEvidenceWrapper
instance ToJSON DesiredEvidenceWrapper
instance ToJSON EvidenceDescriptor
instance FromJSON EvidenceDescriptor
instance ToJSON EvidencePieceWrapper
instance FromJSON EvidencePieceWrapper
--test = DEW [D0, D1,D2,D2]


stripED :: EvidenceDescriptorWrapper -> EvidenceDescriptor
stripED x = evidenceDescriptor x
