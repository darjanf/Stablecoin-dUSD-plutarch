{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Minting (mintingDUSDPolicy) where

import Plutarch
import Plutarch.Num ((#*))
import Plutarch.Api.V2 
import Plutarch.Prelude
import Plutarch.Monadic qualified as P 
import "liqwid-plutarch-extra" Plutarch.Extra.TermCont
import "liqwid-plutarch-extra" Plutarch.Extra.ScriptContext (pfromPDatum)
import Utils (pand'List, pcond, (#>), (#>=), ptryOwnInput, pcountScriptInputs)
import Plutarch.Extra.IsData (EnumIsData (..), PlutusTypeEnumData)
import PlutusLedgerApi.V1
import Plutarch.Api.V1 (PCredential (PPubKeyCredential, PScriptCredential))
import Plutarch.Api.V1.Value
import Plutarch.Api.V1.AssocMap (plookup)
import Plutarch.Unsafe
import Plutarch.DataRepr
import Utils
import Collateral qualified
import Plutarch.Num (pnegate)

---------------------------------------------------------------------------------------------------
----------------------------- ON-CHAIN: CONSTANTS AND DATATYPES -----------------------------------

data PMintParams (s :: S) =
    PMintParams (Term s (PDataRecord 
        '[
            "mpOracleValidator"         ':= PScriptHash
          , "mpCollateralValidator"     ':= PScriptHash
          , "mpCollateralMinPercent"    ':= PInteger
        ]
        ))
    deriving stock (Generic)
    deriving anyclass (PlutusType, PIsData, PDataFields, PShow)

instance DerivePlutusType PMintParams where
    type DPTStrat _ = PlutusTypeData

instance PTryFrom PData PMintParams
instance PTryFrom PData (PAsData PMintParams)

data PMintRedeemer (s :: S) = PMint | PBurn | PLiquidate
 deriving stock (Generic, Enum, Bounded)
 deriving anyclass (PlutusType, PIsData, PEq, PShow)

instance DerivePlutusType PMintRedeemer where
  type DPTStrat _ = PlutusTypeEnumData 

instance PTryFrom PData (PAsData PMintRedeemer)

---------------------------------------------------------------------------------------------------
----------------------------- ON-CHAIN: MINTING SCRIPT --------------------------------------------

-- Stablecoin dUSD minting script
mintingDUSDPolicyT :: Term s (PMintParams :--> PMintRedeemer :--> PScriptContext :--> POpaque)
mintingDUSDPolicyT = phoistAcyclic $ plam $ \mintingParams redeemer ctx -> unTermCont $ do
    --------------------------- General variables ---------------------------
    mintingParamsFields         <- pletFieldsC @'["mpOracleValidator", "mpCollateralValidator", "mpCollateralMinPercent"] mintingParams
    ctxFields                   <- pletFieldsC @'["txInfo", "purpose"] ctx
    ctxInfoFields               <- pletFieldsC @'["inputs", "outputs", "mint", "signatories", "datums"] ctxFields.txInfo
    PMinting purpose            <- pmatchC ctxFields.purpose
    let ownCurrencySymbol       = pfield @"_0" # purpose
    mintedItems                 <- pletC (ptryLookupValue # ownCurrencySymbol # ctxInfoFields.mint)
    mintedTokenPair             <- pletC (phead # mintedItems)
    let _mintedTokenName        = pfromData $ pfstBuiltin # mintedTokenPair
        mintedTokenAmount       = pfromData $ psndBuiltin # mintedTokenPair
        txOutputs               :: Term _ (PBuiltinList PTxOut) 
        txOutputs               = ctxInfoFields.outputs
        adaRate                 :: Term s PInteger
        adaRate                 = 100 -- Rate needs to be read out from RefUTXO         
        collInputs              = pfilter @PBuiltinList
                                # plam (\input -> unTermCont $ do
                                    let inputTxOutAddr  = pfield @"address" # (pfield @"resolved" # input)
                                    PScriptCredential vh    <- pmatchC (pfield @"credential" # inputTxOutAddr)
                                    let inputScriptHash :: Term _ PScriptHash
                                        inputScriptHash = (pfield @"_0" # vh)
                                    pure $ inputScriptHash #== mintingParamsFields.mpCollateralValidator
                                ) # ctxInfoFields.inputs
        collInput               = pheadSingleton #$ collInputs
        collInputValue          = pfield @"value" # (pfield @"resolved" # collInput)
        collInputAmount         = pvalueOf # collInputValue # padaSymbol # padaToken
        maxMintInputAmount      = pdiv # ((pdiv # collInputAmount # mintingParamsFields.mpCollateralMinPercent) #* adaRate) # 1000000
        collOutputDatum         :: Term _ POutputDatum
        collOutputDatum         = pfield @"datum" # (pfield @"resolved" # collInput)
        collDatum               = pparseCollateralDatum # collOutputDatum # ctxInfoFields.datums
    collDatumFields       <- pletFieldsC @'["colMintingPolicyId", "colOwner", "colStablecoinAmount"] collDatum

    --------------------------- Redeemer specific variables ---------------------------
    let checkMintPositive       = mintedTokenAmount #> 0    -- PMint
        checkMintingOutputs     = pany @PBuiltinList        -- PMint
                                # plam (\txo -> unTermCont $ do
                                txOutFields             <- pletFieldsC @'["value", "address", "datum"] txo
                                PScriptCredential vh    <- pmatchC (pfield @"credential" # txOutFields.address)
                                POutputDatum dat        <- pmatchC(txOutFields.datum)
                                let collOutputAmount    = pvalueOf # txOutFields.value # padaSymbol # padaToken
                                    maxMintOutputAmount = pdiv # ((pdiv # collOutputAmount # mintingParamsFields.mpCollateralMinPercent) #* adaRate) # 1000000
                                    checkValidatorHash  = (pfield @"_0" # vh) #== mintingParamsFields.mpCollateralValidator
                                    checkMaxMint        = maxMintOutputAmount #>= mintedTokenAmount
                                    collDatum           = pfromPDatum @Collateral.PCollateralDatum # (pfield @"outputDatum" # dat)
                                collDatumFields         <- pletFieldsC @'["colMintingPolicyId", "colOwner", "colStablecoinAmount"] collDatum
                                let checkCurrencySymbol = collDatumFields.colMintingPolicyId #== ownCurrencySymbol
                                    checkMintedAmount   = collDatumFields.colStablecoinAmount #== mintedTokenAmount
                                    checkOwnerSignature = pelem # collDatumFields.colOwner # ctxInfoFields.signatories
                                pure $ pand'List [checkValidatorHash, checkMaxMint, checkCurrencySymbol, checkMintedAmount, checkOwnerSignature]                       
                                )
                                # txOutputs
        checkBurnNegative       = mintedTokenAmount #< 0    -- PBurn | PLiquidate
        checkBurnMatchesColl    = (pnegate # collDatumFields.colStablecoinAmount) #== mintedTokenAmount -- PBurn | PLiquidate
        checkColOwner           = pelem # collDatumFields.colOwner # ctxInfoFields.signatories -- PBurn
        checkLiquidation        = maxMintInputAmount #< (pnegate # mintedTokenAmount) -- PLiquidate
    pure $ pif ( pmatch redeemer $ \case 
            PMint ->
                pand'List [checkMintPositive, checkMintingOutputs]
            PBurn ->
                pand'List [checkBurnMatchesColl, checkColOwner, checkBurnNegative]
            PLiquidate ->
                pand'List [checkBurnMatchesColl, checkLiquidation, checkBurnNegative]
        )
        (popaque (pconstant ()))
        perror

mintingDUSDPolicy :: ClosedTerm (PData :--> PMintingPolicy)
mintingDUSDPolicy = plam $ \params redeemer ctx -> unTermCont $ do
    (par,_)   <- ptryFromC @(PAsData PMintParams) params
    (redmr,_) <- ptryFromC @(PAsData PMintRedeemer) redeemer
    pure $ popaque $ mintingDUSDPolicyT # pfromData par # pfromData redmr # ctx

---------------------------------------------------------------------------------------------------
----------------------------- ON-CHAIN: HELPER FUNCTIONS --------------------------------------------

pparseCollateralDatum :: Term s (POutputDatum :--> PMap 'Unsorted PDatumHash PDatum :--> Collateral.PCollateralDatum)
pparseCollateralDatum = phoistAcyclic $ plam $ \outputDatum datums -> unTermCont $ do
    pure $ pmatch (outputDatum) $ \case
      PNoOutputDatum _ -> perror
      POutputDatum ((pfield @"outputDatum" #) -> datum) -> unTermCont $ do
        pure $ pfromPDatum @Collateral.PCollateralDatum # datum
      POutputDatumHash ((pfield @"datumHash" #) -> dh) -> unTermCont $ do
        PJust datum <- pmatchC (ptryParseDatum # dh # datums)
        pure $ pfromPDatum @Collateral.PCollateralDatum # datum