{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Collateral where

import Plutarch 
import Plutarch.Api.V2 
import Plutarch.Prelude 
import Plutarch.Monadic qualified as P 
import "liqwid-plutarch-extra" Plutarch.Extra.TermCont 
import Utils (pand'List, pcond, (#>), (#>=), ptryOwnInput, pcountScriptInputs)
import PlutusLedgerApi.V1
import Plutarch.Api.V1.Value
import Plutarch.Api.V1.AssocMap (plookup)
import Plutarch.Unsafe
import Plutarch.DataRepr (PDataFields)
import Plutarch.Lift
import Plutarch.DataRepr
import PlutusTx qualified
import Plutarch.Extra.IsData (EnumIsData (..), PlutusTypeEnumData)
import Plutarch.Num (pnegate)
import Utils

---------------------------------------------------------------------------------------------------
----------------------------- ON-CHAIN: CONSTANTS AND DATATYPES -----------------------------------

stablecoinTokenName :: Term s PByteString 
stablecoinTokenName = pencodeUtf8 # "dUSD"

-- Datum containing all the relevant information
data PCollateralDatum (s :: S) =
    PCollateralDatum (Term s (PDataRecord 
        '[
            "colMintingPolicyId"    ':= PCurrencySymbol
          , "colOwner"              ':= PPubKeyHash
          , "colStablecoinAmount"   ':= PInteger
        ]
        ))
    deriving stock (Generic)
    deriving anyclass (PlutusType, PIsData, PDataFields, PShow)

instance DerivePlutusType PCollateralDatum where
    type DPTStrat _ = PlutusTypeData

instance PTryFrom PData PCollateralDatum
instance PTryFrom PData (PAsData PCollateralDatum)

-- We can lock or redeem our own collateral or liquidate someone else's
data PCollateralRedeemer (s :: S) = PRedeem | PLiquidate
 deriving stock (Generic, Enum, Bounded)
 deriving anyclass (PlutusType, PIsData, PEq, PShow)

instance DerivePlutusType PCollateralRedeemer where
  type DPTStrat _ = PlutusTypeEnumData 

instance PTryFrom PData (PAsData PCollateralRedeemer)

---------------------------------------------------------------------------------------------------
----------------------------- ON-CHAIN: VALIDATOR -------------------------------------------------

collateralValidatorT :: Term s (PCollateralDatum :--> PCollateralRedeemer :--> PScriptContext :--> POpaque)
collateralValidatorT = phoistAcyclic $ plam $ \datum redeemer ctx -> unTermCont $ do
    ctxFields                       <- pletFieldsC @'["txInfo"] ctx
    infoFields                      <- pletFieldsC @'["signatories", "mint"] ctxFields.txInfo
    datumFields                     <- pletFieldsC @'["colMintingPolicyId", "colOwner", "colStablecoinAmount"] datum
    mintedItems                     <- pletC (ptryLookupValue # datumFields.colMintingPolicyId # infoFields.mint)
    let checkSignedByCollOwner      =  (pelem # datumFields.colOwner # infoFields.signatories)
        negCollAmount               = pnegate # datumFields.colStablecoinAmount
        checkMintedItems            = plength # mintedItems #== pconstant 1
        mintedTokenPair             = phead # mintedItems
        mintedTokenAmount           = pfromData $ psndBuiltin # mintedTokenPair
        checkStablecoinAmount       = negCollAmount #== mintedTokenAmount

    pure $ pif ( pmatch redeemer $ \case 
            PRedeem ->
                pand'List [checkSignedByCollOwner, checkMintedItems, checkStablecoinAmount]
            PLiquidate ->
                pand'List [checkStablecoinAmount]
        )
        (popaque (pconstant ()))
        perror

collateralValidator :: ClosedTerm (PValidator)
collateralValidator = plam $ \datum redeemer ctx -> unTermCont $ do
  (dat,_)   <- ptryFromC @(PAsData PCollateralDatum) datum
  (redmr,_) <- ptryFromC @(PAsData PCollateralRedeemer) redeemer
  pure $ popaque $ collateralValidatorT # pfromData dat # pfromData redmr # ctx