{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module PriceFeedOracle (priceFeedOracleValidator) where

import Plutarch 
import Plutarch.Api.V2 
import Plutarch.Api.V1 (PCredential (PPubKeyCredential, PScriptCredential))
import Plutarch.Prelude 
import Plutarch.Monadic qualified as P 
import "liqwid-plutarch-extra" Plutarch.Extra.TermCont 
import Utils (pand'List, pcond, (#>), (#>=), ptryOwnInput, pcountScriptInputs)
import Plutarch.Extra.IsData (EnumIsData (..), PlutusTypeEnumData)
import PlutusLedgerApi.V1
import Plutarch.Api.V1.Value
import Plutarch.Api.V1.AssocMap (plookup)
import Plutarch.Unsafe
import Plutarch.DataRepr
import Plutarch.Extra.Api (pfindOwnInput, pparseDatum)
import Utils
import Plutarch.Maybe (PMaybe (PJust, PNothing))
import Plutarch.Extra.Maybe (pisJust)
import Plutarch.Builtin (pasInt)
---------------------------------------------------------------------------------------------------
----------------------------- ON-CHAIN: CONSTANTS AND DATATYPES -----------------------------------

data POracleParams (s :: S) =
    POracleParams (Term s (PDataRecord 
        '[
            "oNFTCurrencySymbol"    ':= PCurrencySymbol
          , "oNFTTokenName"         ':= PTokenName
          , "oOperator"             ':= PPubKeyHash
        ]
        ))
    deriving stock (Generic)
    deriving anyclass (PlutusType, PIsData, PDataFields, PShow)

instance DerivePlutusType POracleParams where
    type DPTStrat _ = PlutusTypeData

instance PTryFrom PData POracleParams
instance PTryFrom PData (PAsData POracleParams)

data POracleRedeemer (s :: S) = PUpdate | PDelete
 deriving stock (Generic, Enum, Bounded)
 deriving anyclass (PlutusType, PIsData, PEq, PShow)

instance DerivePlutusType POracleRedeemer where
  type DPTStrat _ = PlutusTypeEnumData 

instance PTryFrom PData (PAsData POracleRedeemer)

-- Oracle Datum
type PRate = PInteger

---------------------------------------------------------------------------------------------------
----------------------------- ON-CHAIN: VALIDATOR -------------------------------------------------
priceFeedOracleValidatorT :: Term s (POracleParams :--> PRate :--> POracleRedeemer :--> PScriptContext :--> POpaque)
priceFeedOracleValidatorT = phoistAcyclic $ plam $ \params _datum redeemer ctx -> unTermCont $ do
    ctxFields                                   <- pletFieldsC @'["txInfo", "purpose"] ctx
    txInfoFields                                <- pletFieldsC @'["signatories", "inputs", "outputs", "datums"] ctxFields.txInfo
    paramsFields                                <- pletFieldsC @'["oNFTCurrencySymbol", "oNFTTokenName", "oOperator"] params
    PSpending ((pfield @"_0" #) -> txOutRef)    <- pmatchC (ctxFields.purpose)
    PTxOut ownInput                             <- pmatchC (ptryOwnInput # txInfoFields.inputs # txOutRef)
    PTxOut ownOutput                            <- pmatchC (ptryGetContinuingOutput # txInfoFields.inputs # txInfoFields.outputs # txOutRef)
    let ownInputValue                           = pfield @"value" # ownInput
        ownOutputValue                          = pfield @"value" # ownOutput
        inputNFTAmount                          = pvalueOf # ownInputValue # paramsFields.oNFTCurrencySymbol # paramsFields.oNFTTokenName
        outputNFTAmount                         = pvalueOf # ownOutputValue # paramsFields.oNFTCurrencySymbol # paramsFields.oNFTTokenName
        inputHasToken                           = inputNFTAmount #== pconstant 1
        outputHasToken                          = outputNFTAmount #== pconstant 1
        checkOperatorSignature                  = pelem # paramsFields.oOperator # txInfoFields.signatories
        validOutputDatum                        = pisJust # (ptryParseOracleDatum # (pcon $ PTxOut ownOutput) # txInfoFields.datums)

    pure $ pif ( pmatch redeemer $ \case 
            PUpdate ->
                pand'List [   ptraceIfFalse "token missing from input"   inputHasToken
                            , ptraceIfFalse "token missing from output"  outputHasToken
                            , ptraceIfFalse "operator signature missing" checkOperatorSignature
                            , ptraceIfFalse "invalid output datum"       validOutputDatum
                          ]
            PDelete ->
                pand'List [ ptraceIfFalse "operator signature missing" checkOperatorSignature ]
        )
        (popaque (pconstant ()))
        perror

priceFeedOracleValidator :: ClosedTerm (PData :--> PValidator)
priceFeedOracleValidator = plam $ \params datum redeemer ctx -> unTermCont $ do
  (par,_)   <- ptryFromC @(PAsData POracleParams) params
  (dat,_)   <- ptryFromC @(PAsData PRate) datum
  (redmr,_) <- ptryFromC @(PAsData POracleRedeemer) redeemer
  pure $ popaque $ priceFeedOracleValidatorT # pfromData par # pfromData dat # pfromData redmr # ctx

---------------------------------------------------------------------------------------------------
----------------------------- ON-CHAIN: HELPER FUNCTION -------------------------------------------------

ptryParseOracleDatum :: Term s (PTxOut :--> PMap 'Unsorted PDatumHash PDatum :--> PMaybe PInteger)
ptryParseOracleDatum = phoistAcyclic $ plam $ \output datums -> unTermCont $ do
    pure $ pmatch (pfield @"datum" # output) $ \case
        PNoOutputDatum _ -> pcon $ PNothing
        POutputDatum ((pfield @"outputDatum" #) -> datum) -> unTermCont $ do
            PDatum d <- pmatchC(datum)
            pure $ pcon $ PJust (pasInt # d)
        POutputDatumHash ((pfield @"datumHash" #) -> dh) -> unTermCont $ do
            PJust datum <- pmatchC (ptryParseDatum # dh # datums)
            PDatum d <- pmatchC (datum)
            pure $ pcon $ PJust (pasInt # d)