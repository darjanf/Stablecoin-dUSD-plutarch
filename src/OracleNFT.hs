{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module OracleNFT (oracleNFTPolicy) where

import Plutarch 
import Plutarch.Api.V2 
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
import Utils

---------------------------------------------------------------------------------------------------
----------------------------- ON-CHAIN: CONSTANTS AND DATATYPES -----------------------------------

-- OracleNFT Label
oracleNftLabel :: Term s PByteString 
oracleNftLabel = phexByteStr "4f7261636c654e4654"

---------------------------------------------------------------------------------------------------
----------------------------- ON-CHAIN: MINTING SCRIPT --------------------------------------------

-- OracleNFT minting script
oracleNFTPolicyT :: Term s (PTxOutRef :--> PData :--> PScriptContext :--> POpaque)
oracleNFTPolicyT = phoistAcyclic $ plam $ \utxo _red ctx -> unTermCont $ do
    ctxFields               <- pletFieldsC @'["txInfo", "purpose"] ctx
    ctxInfoFields           <- pletFieldsC @'["inputs", "mint"] ctxFields.txInfo
    PMinting purpose        <- pmatchC ctxFields.purpose
    -- or PMinting ((pfield @"_0" #) -> ownCurrencySymbol)
    let ownCurrencySymbol   = pfield @"_0" # purpose
    mintedItems             <- pletC (ptryLookupValue # ownCurrencySymbol # ctxInfoFields.mint)
    mintedTokenPair         <- pletC (phead # mintedItems)
    let checkMintedAmount   = plength # mintedItems #== pconstant 1
        mintedTokenName     = pfromData $ pfstBuiltin # mintedTokenPair
        mintedTokenAmount   = pfromData $ psndBuiltin # mintedTokenPair
        checkTokenAmount    = mintedTokenAmount #== pconstant 1
    PPair tokenLabel _      <- pmatchC (pbreakTokenName mintedTokenName)
    checkTokenName          <- pletC (pto $ tokenLabel #== oracleNftLabel)
    hasUtxo                 <- pletC (
                                pany @PBuiltinList 
                                # plam (\txIn -> let txInRef = (pfield @"outRef" # txIn) in txInRef #== utxo)
                                # ctxInfoFields.inputs
                                )
    let checks              = pand'List [hasUtxo,checkTokenName,checkMintedAmount,checkTokenAmount]

    pure $ pif checks
        (popaque (pconstant ()))
        perror

oracleNFTPolicy :: ClosedTerm (PTxOutRef :--> PMintingPolicy)
oracleNFTPolicy = plam $ \utxo redeemer ctx -> unTermCont $ do
  pure $ popaque $ oracleNFTPolicyT # utxo # redeemer # ctx