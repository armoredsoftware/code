{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module JSONCaster where

import Demo2Shared as D2
import Data.Aeson
import Data.Aeson.TH
import Data.Map
import Data.Text
import Data.ByteString.Lazy
--import Data.ByteString.Internal as IB
data DesiredEvidenceWrapper = DEW {desiredEvidence :: DesiredEvidence} deriving (Show)
data EvidenceDescriptorWrapper = EDW {evidenceDescriptor :: EvidenceDescriptor} deriving ( Show)
data EvidencePieceWrapper = EPW {evidencePiece :: EvidencePiece} deriving (Show)

$(deriveJSON defaultOptions ''EvidenceDescriptor)
$(deriveJSON defaultOptions ''EvidenceDescriptorWrapper)
$(deriveJSON defaultOptions ''DesiredEvidenceWrapper)
--test = DEW [D0, D1,D2,D2]


stripED :: EvidenceDescriptorWrapper -> EvidenceDescriptor
stripED x = evidenceDescriptor x
